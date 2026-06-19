package app.softnetwork.payment.service

import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.audit.PaymentAuditLog.audit
import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  CreateOrUpdateKycDocument,
  CustomerUpdated,
  DisablePaymentMethodFromWebhook,
  InvalidateRegularUser,
  KycDocumentCreatedOrUpdated,
  PaymentMethodDisabled,
  PaymentMethodRegistered,
  RecurringPaymentCallback,
  RegisterPaymentMethodFromWebhook,
  RegularUserInvalidated,
  RegularUserValidated,
  UpdateCustomerFromWebhook,
  UpdateRecurringCardPaymentRegistration,
  ValidateRegularUser
}
import app.softnetwork.payment.model.{Address, KycDocument, RecurringPayment}
import com.stripe.model.{
  Account,
  Customer,
  Event,
  Invoice,
  PaymentMethod => StripePaymentMethod,
  Person,
  StripeObject,
  Subscription
}
import com.stripe.net.Webhook

import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

/** Created by smanciot on 27/04/2021.
  */
trait StripeEventHandler extends Completion { _: BasicPaymentService with PaymentHandler =>

  /** Bounded LRU set of recently processed webhook event IDs for idempotency. Stripe may deliver
    * the same event multiple times. Uses a synchronized LinkedHashMap configured as LRU to evict
    * oldest entries first.
    */
  private[this] val MaxProcessedEventIds = 10000

  private[this] val processedEventIds: java.util.Map[String, java.lang.Boolean] =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[String, java.lang.Boolean](256, 0.75f, true) {
        override def removeEldestEntry(
          eldest: java.util.Map.Entry[String, java.lang.Boolean]
        ): Boolean =
          size() > MaxProcessedEventIds
      }
    )

  private[this] def isEventAlreadyProcessed(eventId: String): Boolean = {
    if (processedEventIds.containsKey(eventId)) {
      true
    } else {
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
        // Save the payload to a file for replaying events that failed to be processed.
        StripeApi.writeFailedEvent(payload)
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
    // Story 13.7 — audit the inbound webhook; the Stripe event id is its natural correlation key.
    audit.event(event.getId, "webhook_received", "stripe_event_type" -> event.getType)
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
              val cmd = InvalidateRegularUser(accountId)
              cmd.withCorrelationId(resolveCorrelationId(account, event)) // Story 13.7
              run(cmd).complete() match {
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
                                reason,
                                event.getId
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
                                reason,
                                event.getId
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
                                reason,
                                event.getId
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
              val cmd = ValidateRegularUser(account.getId)
              cmd.withCorrelationId(resolveCorrelationId(account, event)) // Story 13.7
              run(cmd).complete() match {
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
                              reason,
                              event.getId
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
                              reason,
                              event.getId
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
            val cmd = RecurringPaymentCallback(
              subscriptionId,
              transactionId,
              Some(customerId)
            )
            cmd.withCorrelationId(resolveCorrelationId(invoice, event)) // Story 13.7
            run(cmd).complete() match {
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
            val cmd =
              RecurringPaymentCallback(subscriptionId, transactionId, Some(customerId))
            cmd.withCorrelationId(resolveCorrelationId(invoice, event)) // Story 13.7
            run(cmd).complete() match {
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
          val cmd =
            UpdateRecurringCardPaymentRegistration(
              customerId,
              subscriptionId,
              status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
            )
          cmd.withCorrelationId(resolveCorrelationId(subscription, event)) // Story 13.7
          run(cmd).complete() match {
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
              val cmd =
                UpdateRecurringCardPaymentRegistration(
                  customerId,
                  subscriptionId,
                  status = Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
                )
              cmd.withCorrelationId(resolveCorrelationId(subscription, event)) // Story 13.7
              run(cmd).complete() match {
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

      case "payment_method.attached" =>
        log.info(s"[Payment Hooks] Webhook: Payment Method Attached")
        val maybePm: Option[StripePaymentMethod] =
          extractStripeObject[StripePaymentMethod](event)
        maybePm.foreach { pm =>
          val paymentMethodId = pm.getId
          val customerId = pm.getCustomer
          log.info(
            s"[Payment Hooks] Payment method $paymentMethodId attached to customer $customerId"
          )
          val cmd = RegisterPaymentMethodFromWebhook(customerId, paymentMethodId)
          cmd.withCorrelationId(resolveCorrelationId(pm, event)) // Story 13.7
          run(cmd).complete() match {
            case Success(_: PaymentMethodRegistered.type) =>
              log.info(
                s"[Payment Hooks] Payment method $paymentMethodId registered for $customerId"
              )
            case Success(other) =>
              log.warn(
                s"[Payment Hooks] Unexpected result for payment method attach: $other"
              )
            case Failure(f) =>
              log.error(
                s"[Payment Hooks] Failed to register payment method $paymentMethodId: ${f.getMessage}",
                f
              )
          }
        }

      case "payment_method.detached" =>
        log.info(s"[Payment Hooks] Webhook: Payment Method Detached")
        val maybePm: Option[StripePaymentMethod] =
          extractStripeObject[StripePaymentMethod](event)
        maybePm.foreach { pm =>
          val paymentMethodId = pm.getId
          // After detach, pm.getCustomer() is null. Retrieve the customer ID from
          // previous_attributes where Stripe stores the pre-detach state.
          val customerId = Option(pm.getCustomer).orElse {
            Option(event.getData.getPreviousAttributes)
              .flatMap(prev => Option(prev.get("customer")).map(_.toString))
          }
          customerId match {
            case Some(cid) =>
              log.info(
                s"[Payment Hooks] Payment method $paymentMethodId detached from customer $cid"
              )
              val cmd = DisablePaymentMethodFromWebhook(cid, paymentMethodId)
              cmd.withCorrelationId(resolveCorrelationId(pm, event)) // Story 13.7
              run(cmd).complete() match {
                case Success(_: PaymentMethodDisabled.type) =>
                  log.info(s"[Payment Hooks] Payment method $paymentMethodId disabled")
                case Success(other) =>
                  log.warn(
                    s"[Payment Hooks] Unexpected result for payment method detach: $other"
                  )
                case Failure(f) =>
                  log.error(
                    s"[Payment Hooks] Failed to disable payment method $paymentMethodId: ${f.getMessage}",
                    f
                  )
              }
            case None =>
              log.warn(
                s"[Payment Hooks] Cannot disable payment method $paymentMethodId: " +
                "customer ID not found in event data or previous_attributes"
              )
          }
        }

      case "customer.updated" =>
        log.info(s"[Payment Hooks] Webhook: Customer Updated")
        val maybeCustomer: Option[Customer] = extractStripeObject[Customer](event)
        maybeCustomer.foreach { customer =>
          val customerId = customer.getId
          log.info(s"[Payment Hooks] Customer $customerId updated")
          val address = Option(customer.getAddress).map { a =>
            Address.defaultInstance
              .withAddressLine(
                Seq(Option(a.getLine1), Option(a.getLine2)).flatten.mkString(", ")
              )
              .withCity(Option(a.getCity).getOrElse(""))
              .withPostalCode(Option(a.getPostalCode).getOrElse(""))
              .withCountry(Option(a.getCountry).getOrElse(""))
              .copy(state = Option(a.getState))
          }
          val cmd =
            UpdateCustomerFromWebhook(
              customerId,
              name = Option(customer.getName),
              email = Option(customer.getEmail),
              phone = Option(customer.getPhone),
              address = address
            )
          cmd.withCorrelationId(resolveCorrelationId(customer, event)) // Story 13.7
          run(cmd).complete() match {
            case Success(_: CustomerUpdated.type) =>
              log.info(s"[Payment Hooks] Customer $customerId info updated")
            case Success(other) =>
              log.warn(s"[Payment Hooks] Unexpected result for customer update: $other")
            case Failure(f) =>
              log.error(
                s"[Payment Hooks] Failed to update customer $customerId: ${f.getMessage}",
                f
              )
          }
        }

      case _ =>
        log.info(s"[Payment Hooks] Stripe Webhook received: ${event.getType}")
    }
  }

  /** Story 13.7 — read the metadata map off any Stripe object that exposes `getMetadata` (most do:
    * PaymentIntent, Subscription, Invoice, Customer, PaymentMethod, …). Reflection keeps this
    * generic since the Stripe Java SDK has no common metadata interface. Failure is benign → empty
    * map.
    */
  private[this] def stripeMetadata(obj: StripeObject): Map[String, String] =
    Try {
      obj.getClass.getMethod("getMetadata").invoke(obj) match {
        case m: java.util.Map[_, _] =>
          m.asScala.collect { case (k: String, v: String) => k -> v }.toMap
        case _ => Map.empty[String, String]
      }
    }.getOrElse(Map.empty[String, String])

  /** Story 13.7 — the subscription id that backs this object, used as a correlation fallback: a
    * recurring registration with no explicit cid stamps the subscription id as its correlation id
    * (cf `RecurringPaymentCommandHandler`), so the webhook must resolve to the same value.
    */
  private[this] def subscriptionId(obj: StripeObject): Option[String] =
    obj match {
      case s: Subscription => Option(s.getId).filter(_.nonEmpty)
      case i: Invoice      => Option(i.getSubscription).filter(_.nonEmpty)
      case _               => None
    }

  /** Story 13.7 — resolve the cross-service correlation id for a webhook-driven command (READ side
    * of the softpayment <-> Stripe round-trip). The provider writes the id into the Stripe object
    * metadata at creation (`correlation_id`); otherwise fall back, in order, to the
    * `external_reference` / `order_uuid` it also stamps, then the backing subscription id
    * (recurring), then the event id.
    */
  private[this] def resolveCorrelationId(obj: StripeObject, event: Event): String = {
    val metadata = stripeMetadata(obj)
    metadata
      .get("correlation_id")
      .filter(_.nonEmpty)
      .orElse(metadata.get("external_reference").filter(_.nonEmpty))
      .orElse(metadata.get("order_uuid").filter(_.nonEmpty))
      .orElse(subscriptionId(obj))
      .getOrElse(event.getId)
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
    reason: String,
    correlationId: String // Story 13.7 — Stripe event id, threaded from handleStripeEvent
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
    val cmd = CreateOrUpdateKycDocument(accountId, document)
    cmd.withCorrelationId(correlationId) // Story 13.7 — webhook cid = Stripe event id
    run(cmd).complete() match {
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
