package app.softnetwork.payment.service

import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  CreateOrUpdateKycDocument,
  InvalidateRegularUser,
  KycDocumentCreatedOrUpdated,
  RecurringPaymentCallback,
  RegularUserInvalidated,
  RegularUserValidated,
  UpdateRecurringCardPaymentRegistration,
  ValidateRegularUser
}
import app.softnetwork.payment.model.{KycDocument, RecurringPayment}
import com.stripe.model.{Account, Event, Invoice, Person, StripeObject, Subscription}
import com.stripe.net.Webhook

import java.util.concurrent.ConcurrentHashMap

import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

/** Created by smanciot on 27/04/2021.
  */
trait StripeEventHandler extends Completion { _: BasicPaymentService with PaymentHandler =>

  /** Bounded set of recently processed webhook event IDs for idempotency. Stripe may deliver the
    * same event multiple times.
    */
  private[this] val processedEventIds: ConcurrentHashMap[String, java.lang.Boolean] =
    new ConcurrentHashMap[String, java.lang.Boolean](256)

  private[this] val MaxProcessedEventIds = 10000

  private[this] def isEventAlreadyProcessed(eventId: String): Boolean = {
    if (processedEventIds.containsKey(eventId)) {
      true
    } else {
      if (processedEventIds.size() >= MaxProcessedEventIds) {
        // Evict oldest entries (ConcurrentHashMap has no LRU, so clear half)
        val iterator = processedEventIds.keys()
        var count = 0
        while (iterator.hasMoreElements && count < MaxProcessedEventIds / 2) {
          processedEventIds.remove(iterator.nextElement())
          count += 1
        }
      }
      processedEventIds.put(eventId, java.lang.Boolean.TRUE)
      false
    }
  }

  def toStripeEvent(payload: String, sigHeader: String, secret: String): Option[Event] = {
    Try {
      Webhook.constructEvent(payload, sigHeader, secret)
    } match {
      case Success(event) =>
        Some(event)
      case Failure(f) =>
        log.error(s"[Payment Hooks] Stripe Webhook verification failed: ${f.getMessage}", f)
        log.error(payload)
        None
    }
  }

  def handleStripeEvent(event: Event): Unit = {
    if (isEventAlreadyProcessed(event.getId)) {
      log.info(
        s"[Payment Hooks] Skipping duplicate Stripe event: ${event.getId} (${event.getType})"
      )
      return
    }
    event.getType match {

      case "account.updated" =>
        log.info(s"[Payment Hooks] Stripe Webhook received: Account Updated")
        val maybeAccount: Option[Account] = extractStripeObject[Account](event)
        maybeAccount match {
          case Some(account) =>
            val accountId = account.getId
            log.info(
              s"[Payment Hooks] Stripe Webhook received: Account Updated -> $accountId"
            )
            if (!account.getChargesEnabled || !account.getPayoutsEnabled) {
              Option(account.getRequirements.getDisabledReason) match {
                case Some(reason) =>
                  log.info(
                    s"[Payment Hooks] Stripe Webhook received: Account Updated -> Charges and/or Payouts are disabled for $accountId -> $reason"
                  )
                case None =>
                  log.info(
                    s"[Payment Hooks] Stripe Webhook received: Account Updated -> Charges and/or Payouts are disabled for $accountId"
                  )
              }
              //disable account
              run(InvalidateRegularUser(accountId)).complete() match {
                case Success(RegularUserInvalidated) =>
                  log.info(
                    s"[Payment Hooks] Stripe Webhook received: Account Updated -> Account disabled for $accountId"
                  )
                  // update KYC document(s) status for individual
                  Option(account.getRequirements)
                    .map(_.getErrors.asScala.toSeq)
                    .getOrElse(Seq.empty)
                    .foreach { error =>
                      val requirement = error.getRequirement
                      val code = error.getCode
                      val reason = error.getReason
                      log.warn(
                        s"[Payment Hooks] Stripe Webhook received: Account Updated $requirement : $code -> $reason"
                      )
                      requirement match {
                        case "verification.document.front" | "verification.document.back" =>
                          Option(account.getIndividual).map(_.getVerification.getDocument) match {
                            case Some(document) if Option(document.getFront).isDefined =>
                              val front = document.getFront
                              val documentId = Option(
                                document.getBack
                              ) match {
                                case Some(back) => s"$front#$back"
                                case _          => front
                              }
                              refuseDocument(
                                accountId,
                                documentId,
                                KycDocument.KycDocumentType.KYC_IDENTITY_PROOF,
                                code,
                                reason
                              )
                            case _ =>
                          }
                        case "verification.additional_document.front" |
                            "verification.additional_document.back" =>
                          Option(account.getIndividual).flatMap(individual =>
                            Option(individual.getVerification.getAdditionalDocument)
                          ) match {
                            case Some(document) if Option(document.getFront).isDefined =>
                              val front = document.getFront
                              val documentId = Option(
                                document.getBack
                              ) match {
                                case Some(back) => s"$front#$back"
                                case _          => front
                              }
                              refuseDocument(
                                accountId,
                                documentId,
                                KycDocument.KycDocumentType.KYC_ADDRESS_PROOF,
                                code,
                                reason
                              )
                            case _ =>
                          }
                        case "company.verification.document" =>
                          Option(account.getCompany)
                            .flatMap(company => Option(company.getVerification))
                            .map(_.getDocument) match {
                            case Some(document) if Option(document.getFront).isDefined =>
                              val front = document.getFront
                              val documentId = Option(
                                document.getBack
                              ) match {
                                case Some(back) => s"$front#$back"
                                case _          => front
                              }
                              refuseDocument(
                                accountId,
                                documentId,
                                KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
                                code,
                                reason
                              )
                            case _ =>
                          }
                        case _ =>
                      }
                    }

                case _ =>
                  log.warn(
                    s"[Payment Hooks] Stripe Webhook received: Account Updated -> Account not disabled for $accountId"
                  )
              }
            } else {
              log.info(
                s"[Payment Hooks] Stripe Webhook received: Account Updated -> Charges and Payouts are enabled for $accountId"
              )
              //enable account
              run(ValidateRegularUser(account.getId)).complete() match {
                case Success(RegularUserValidated) =>
                  log.info(
                    s"[Payment Hooks] Stripe Webhook received: Account Updated -> Account enabled for $accountId"
                  )
                case _ =>
                  log.warn(
                    s"[Payment Hooks] Stripe Webhook received: Account Updated -> Account not enabled for $accountId"
                  )
              }
            }

          case None =>
            log.warn(s"[Payment Hooks] Stripe Webhook received: Account Updated -> No data")
        }

      case "person.updated" =>
        log.info(s"[Payment Hooks] Stripe Webhook received: Person Updated")
        val maybePerson: Option[Person] = extractStripeObject[Person](event)
        maybePerson match {
          case Some(person) =>
            val personId = person.getId
            val accountId = person.getAccount
            log.info(
              s"[Payment Hooks] Stripe Webhook received: Person Updated -> $personId"
            )
            val verification = person.getVerification
            // update KYC document(s) status for person
            verification.getStatus match {
              case "unverified" =>
                log.info(
                  s"[Payment Hooks] Stripe Webhook received: Person Updated -> Person unverified -> $personId"
                )
                Option(person.getRequirements)
                  .map(_.getErrors.asScala.toSeq)
                  .getOrElse(Seq.empty)
                  .foreach { error =>
                    val requirement = error.getRequirement
                    val code = error.getCode
                    val reason = error.getReason
                    log.warn(
                      s"[Payment Hooks] Stripe Webhook received: Person Updated $requirement : $code -> $reason"
                    )
                    requirement match {
                      case "verification.document.front" | "verification.document.back" =>
                        Option(verification.getDocument) match {
                          case Some(document) if Option(document.getFront).isDefined =>
                            val front = document.getFront
                            val documentId = Option(
                              document.getBack
                            ) match {
                              case Some(back) => s"$front#$back"
                              case _          => front
                            }
                            refuseDocument(
                              accountId,
                              documentId,
                              KycDocument.KycDocumentType.KYC_IDENTITY_PROOF,
                              code,
                              reason
                            )
                          case _ =>
                        }
                      case "verification.additional_document.front" |
                          "verification.additional_document.back" =>
                        Option(verification.getAdditionalDocument) match {
                          case Some(document) if Option(document.getFront).isDefined =>
                            val front = document.getFront
                            val documentId = Option(
                              document.getBack
                            ) match {
                              case Some(back) => s"$front#$back"
                              case _          => front
                            }
                            refuseDocument(
                              accountId,
                              documentId,
                              KycDocument.KycDocumentType.KYC_ADDRESS_PROOF,
                              code,
                              reason
                            )
                          case _ =>
                        }
                      case _ =>
                    }
                  }
              case "verified" =>
                log.info(
                  s"[Payment Hooks] Stripe Webhook received: Person Updated -> Person verified -> $personId"
                )
              case _ =>
            }
          case None =>
            log.warn(s"[Payment Hooks] Stripe Webhook received: Person Updated -> No data")
        }

      case "invoice.payment_succeeded" =>
        log.info(s"[Payment Hooks] Stripe Webhook: Invoice Payment Succeeded")
        val maybeInvoice: Option[Invoice] = extractStripeObject[Invoice](event)
        maybeInvoice.foreach { invoice =>
          Option(invoice.getSubscription).foreach { subscriptionId =>
            val transactionId = Option(invoice.getPaymentIntent).getOrElse(invoice.getId)
            val customerId = invoice.getCustomer
            log.info(
              s"[Payment Hooks] Subscription $subscriptionId invoice paid: $transactionId"
            )
            run(
              RecurringPaymentCallback(subscriptionId, transactionId, Some(customerId))
            ).complete() match {
              case Success(_) =>
                log.info(
                  s"[Payment Hooks] Recurring payment callback processed: $subscriptionId"
                )
              case Failure(f) =>
                log.error(
                  s"[Payment Hooks] Failed to process recurring payment callback: ${f.getMessage}",
                  f
                )
            }
          }
        }

      case "invoice.payment_failed" =>
        log.warn(s"[Payment Hooks] Stripe Webhook: Invoice Payment Failed")
        val maybeInvoice: Option[Invoice] = extractStripeObject[Invoice](event)
        maybeInvoice.foreach { invoice =>
          Option(invoice.getSubscription).foreach { subscriptionId =>
            val transactionId = Option(invoice.getPaymentIntent).getOrElse(invoice.getId)
            val customerId = invoice.getCustomer
            log.warn(
              s"[Payment Hooks] Subscription $subscriptionId invoice payment failed: $transactionId"
            )
            run(
              RecurringPaymentCallback(subscriptionId, transactionId, Some(customerId))
            ).complete() match {
              case Success(_) =>
                log.info(
                  s"[Payment Hooks] Payment failure callback processed for $subscriptionId"
                )
              case Failure(f) =>
                log.error(
                  s"[Payment Hooks] Failed to process payment failure callback for $subscriptionId: ${f.getMessage}",
                  f
                )
            }
          }
        }

      case "customer.subscription.deleted" =>
        log.info(s"[Payment Hooks] Stripe Webhook: Subscription Deleted")
        val maybeSubscription: Option[Subscription] = extractStripeObject[Subscription](event)
        maybeSubscription.foreach { subscription =>
          val subscriptionId = subscription.getId
          val customerId = subscription.getCustomer
          log.info(
            s"[Payment Hooks] Subscription $subscriptionId deleted for customer $customerId"
          )
          run(
            UpdateRecurringCardPaymentRegistration(
              customerId,
              subscriptionId,
              status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
            )
          ).complete() match {
            case Success(_) =>
              log.info(s"[Payment Hooks] Subscription $subscriptionId marked as ENDED")
            case Failure(f) =>
              log.error(
                s"[Payment Hooks] Failed to end subscription $subscriptionId: ${f.getMessage}",
                f
              )
          }
        }

      case "customer.subscription.updated" =>
        log.info(s"[Payment Hooks] Stripe Webhook: Subscription Updated")
        val maybeSubscription: Option[Subscription] = extractStripeObject[Subscription](event)
        maybeSubscription.foreach { subscription =>
          val subscriptionId = subscription.getId
          val customerId = subscription.getCustomer
          val stripeStatus = subscription.getStatus
          log.info(
            s"[Payment Hooks] Subscription $subscriptionId updated: status=$stripeStatus"
          )
          // If subscription moved to a terminal state, sync local state
          stripeStatus match {
            case "canceled" | "incomplete_expired" | "paused" =>
              run(
                UpdateRecurringCardPaymentRegistration(
                  customerId,
                  subscriptionId,
                  status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
                )
              ).complete() match {
                case Success(_) =>
                  log.info(s"[Payment Hooks] Subscription $subscriptionId synced to ENDED")
                case Failure(f) =>
                  log.error(
                    s"[Payment Hooks] Failed to sync subscription $subscriptionId: ${f.getMessage}",
                    f
                  )
              }
            case _ => // Non-terminal status changes are informational
          }
        }

      case _ =>
        log.info(s"[Payment Hooks] Stripe Webhook received: ${event.getType}")
    }
  }

  private[this] def extractStripeObject[T <: StripeObject: scala.reflect.ClassTag](
    event: Event
  ): Option[T] = {
    val ct = scala.reflect.classTag[T]
    Option(event.getDataObjectDeserializer.getObject.orElse(null)).flatMap {
      case obj if ct.runtimeClass.isInstance(obj) => Some(obj.asInstanceOf[T])
      case obj =>
        log.warn(
          s"[Payment Hooks] Expected ${ct.runtimeClass.getSimpleName} but got ${obj.getClass.getSimpleName}"
        )
        None
    }
  }

  private[this] def refuseDocument(
    accountId: String,
    documentId: String,
    documentType: KycDocument.KycDocumentType,
    code: String,
    reason: String
  ): Unit = {
    log.warn(
      s"[Payment Hooks] Stripe Webhook received: Document ID: $documentId refused"
    )
    val document = KycDocument.defaultInstance
      .withId(documentId)
      .withType(documentType)
      .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED)
      .withRefusedReasonType(code)
      .withRefusedReasonMessage(reason)
    run(
      CreateOrUpdateKycDocument(
        accountId,
        document
      )
    ).complete() match {
      case Success(KycDocumentCreatedOrUpdated) =>
        log.info(
          s"[Payment Hooks] Stripe Webhook received: Document ID: $documentId refused"
        )
      case _ =>
        log.warn(
          s"[Payment Hooks] Stripe Webhook received: Document ID: $documentId not updated"
        )
    }
  }
}
