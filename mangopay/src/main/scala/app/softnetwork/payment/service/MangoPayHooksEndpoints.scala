package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{BankAccount, KycDocument, UboDeclaration}
import com.mangopay.core.enumerations.EventType
import sttp.tapir.Endpoint
import sttp.tapir.server.ServerEndpoint.Full

import scala.concurrent.Future

trait MangoPayHooksEndpoints extends HooksEndpoints with PaymentHandler {

  /** should be implemented by each payment provider
    */
  def hooks(
    rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any]
  ): Full[Unit, Unit, _, Unit, Unit, Any, Future] =
    rootEndpoint
      .description("MangoPay Payment Hooks")
      .in(query[String]("EventType").description("MangoPay Event Type"))
      .in(query[String]("RessourceId").description("MangoPay Resource Id related to this event"))
      .serverLogic { case (eventType, resourceId) =>
        Option(EventType.valueOf(eventType)) match {
          case Some(s) =>
            s match {
              case EventType.KYC_FAILED =>
                run(
                  UpdateKycDocumentStatus(
                    resourceId,
                    Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED)
                  )
                ).map {
                  case _: KycDocumentStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] KYC has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] KYC has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.KYC_SUCCEEDED =>
                run(
                  UpdateKycDocumentStatus(
                    resourceId,
                    Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED)
                  )
                ).map {
                  case _: KycDocumentStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] KYC has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] KYC has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.KYC_OUTDATED =>
                run(
                  UpdateKycDocumentStatus(
                    resourceId,
                    Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_OUT_OF_DATE)
                  )
                ).map {
                  case _: KycDocumentStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] KYC has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] KYC has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.UBO_DECLARATION_REFUSED =>
                run(
                  UpdateUboDeclarationStatus(
                    resourceId,
                    Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_REFUSED)
                  )
                ).map {
                  case UboDeclarationStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Ubo Declaration has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Ubo Declaration has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.UBO_DECLARATION_VALIDATED =>
                run(
                  UpdateUboDeclarationStatus(
                    resourceId,
                    Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED)
                  )
                ).map {
                  case UboDeclarationStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Ubo Declaration has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Ubo Declaration has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.UBO_DECLARATION_INCOMPLETE =>
                run(
                  UpdateUboDeclarationStatus(
                    resourceId,
                    Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_INCOMPLETE)
                  )
                ).map {
                  case UboDeclarationStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Ubo Declaration has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Ubo Declaration has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.USER_KYC_REGULAR =>
                run(ValidateRegularUser(resourceId)).map {
                  case RegularUserValidated =>
                    log.info(
                      s"[Payment Hooks] Regular User has been validated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Regular User has not been validated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.USER_KYC_LIGHT =>
                run(InvalidateRegularUser(resourceId)).map {
                  case RegularUserValidated =>
                    log.info(
                      s"[Payment Hooks] Regular User has been validated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Regular User has not been validated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.MANDATE_FAILED =>
                run(
                  UpdateMandateStatus(resourceId, Some(BankAccount.MandateStatus.MANDATE_FAILED))
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.MANDATE_SUBMITTED =>
                run(
                  UpdateMandateStatus(
                    resourceId,
                    Some(BankAccount.MandateStatus.MANDATE_SUBMITTED)
                  )
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.MANDATE_CREATED =>
                run(
                  UpdateMandateStatus(resourceId, Some(BankAccount.MandateStatus.MANDATE_CREATED))
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.MANDATE_EXPIRED =>
                run(
                  UpdateMandateStatus(resourceId, Some(BankAccount.MandateStatus.MANDATE_EXPIRED))
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case EventType.MANDATE_ACTIVATED =>
                run(
                  UpdateMandateStatus(
                    resourceId,
                    Some(BankAccount.MandateStatus.MANDATE_ACTIVATED)
                  )
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right(())
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right(())
                }
              case _ =>
                log.error(s"[Payment Hooks] Event $eventType for $resourceId is not supported")
                Future.successful(Left(()))
            }
          case None =>
            log.error(s"[Payment Hooks] Event $eventType for $resourceId is not supported")
            Future.successful(Left(()))
        }
      }

}
