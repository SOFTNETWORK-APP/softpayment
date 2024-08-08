package app.softnetwork.payment.service

import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  CreateOrUpdateKycDocument,
  InvalidateRegularUser,
  KycDocumentCreatedOrUpdated,
  RegularUserInvalidated,
  RegularUserValidated,
  ValidateRegularUser
}
import app.softnetwork.payment.model.KycDocument
import com.stripe.model.{Account, Event, Person, StripeObject}
import com.stripe.net.Webhook

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions
import collection.JavaConverters._

/** Created by smanciot on 27/04/2021.
  */
trait StripeEventHandler extends Completion { _: BasicPaymentService with PaymentHandler =>

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
    event.getType match {

      case "account.updated" =>
        log.info(s"[Payment Hooks] Stripe Webhook received: Account Updated")
        val maybeAccount: Option[Account] = event
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
              run(InvalidateRegularUser(accountId)) complete () match {
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
              run(ValidateRegularUser(account.getId)) complete () match {
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
        val maybePerson: Option[Person] = event
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

      case _ =>
        log.info(s"[Payment Hooks] Stripe Webhook received: ${event.getType}")
    }
  }

  implicit def toStripeObject[T <: StripeObject](event: Event): Option[T] = {
    Option(event.getDataObjectDeserializer.getObject.orElse(null)).map(_.asInstanceOf[T])
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
    ) complete () match {
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
