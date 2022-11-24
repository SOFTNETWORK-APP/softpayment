package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.config.PaymentSettings.HooksRoute
import app.softnetwork.payment.handlers.MangoPayPaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{KycDocumentStatusUpdated, MandateStatusUpdated, RegularUserValidated, UboDeclarationStatusUpdated, UpdateKycDocumentStatus, UpdateMandateStatus, UpdateUboDeclarationStatus, ValidateRegularUser}
import app.softnetwork.payment.model.{BankAccount, KycDocument, UboDeclaration}
import com.mangopay.core.enumerations.EventType

trait MangoPayPaymentService extends GenericPaymentService with MangoPayPaymentHandler{
  override lazy val hooks: Route = pathPrefix(HooksRoute){
    pathEnd{
      parameters("EventType", "RessourceId") {(eventType, ressourceId) =>
        Option(EventType.valueOf(eventType)) match {
          case Some(s) =>
            s match {
              case EventType.KYC_FAILED =>
                run(UpdateKycDocumentStatus(ressourceId, Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED))) completeWith {
                  case _: KycDocumentStatusUpdated =>
                    logger.info(s"[Payment Hooks] KYC has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] KYC has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.KYC_SUCCEEDED =>
                run(UpdateKycDocumentStatus(ressourceId, Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED))) completeWith {
                  case _: KycDocumentStatusUpdated =>
                    logger.info(s"[Payment Hooks] KYC has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] KYC has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.KYC_OUTDATED =>
                run(UpdateKycDocumentStatus(ressourceId, Some(KycDocument.KycDocumentStatus.KYC_DOCUMENT_OUT_OF_DATE))) completeWith {
                  case _: KycDocumentStatusUpdated =>
                    logger.info(s"[Payment Hooks] KYC has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] KYC has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.UBO_DECLARATION_REFUSED =>
                run(UpdateUboDeclarationStatus(ressourceId, Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_REFUSED))) completeWith {
                  case UboDeclarationStatusUpdated =>
                    logger.info(s"[Payment Hooks] Ubo Declaration has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Ubo Declaration has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.UBO_DECLARATION_VALIDATED =>
                run(UpdateUboDeclarationStatus(ressourceId, Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED))) completeWith {
                  case UboDeclarationStatusUpdated =>
                    logger.info(s"[Payment Hooks] Ubo Declaration has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Ubo Declaration has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.UBO_DECLARATION_INCOMPLETE =>
                run(UpdateUboDeclarationStatus(ressourceId, Some(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_INCOMPLETE))) completeWith {
                  case UboDeclarationStatusUpdated =>
                    logger.info(s"[Payment Hooks] Ubo Declaration has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Ubo Declaration has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.USER_KYC_REGULAR =>
                run(ValidateRegularUser(ressourceId)) completeWith {
                  case RegularUserValidated =>
                    logger.info(s"[Payment Hooks] Regular User has been validated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Regular User has not been validated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.MANDATE_FAILED =>
                run(UpdateMandateStatus(ressourceId, Some(BankAccount.MandateStatus.MANDATE_FAILED))) completeWith {
                  case _: MandateStatusUpdated =>
                    logger.info(s"[Payment Hooks] Mandate has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Mandate has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.MANDATE_SUBMITTED =>
                run(UpdateMandateStatus(ressourceId, Some(BankAccount.MandateStatus.MANDATE_SUBMITTED))) completeWith {
                  case _: MandateStatusUpdated =>
                    logger.info(s"[Payment Hooks] Mandate has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Mandate has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.MANDATE_CREATED =>
                run(UpdateMandateStatus(ressourceId, Some(BankAccount.MandateStatus.MANDATE_CREATED))) completeWith {
                  case _: MandateStatusUpdated =>
                    logger.info(s"[Payment Hooks] Mandate has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Mandate has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.MANDATE_EXPIRED =>
                run(UpdateMandateStatus(ressourceId, Some(BankAccount.MandateStatus.MANDATE_EXPIRED))) completeWith {
                  case _: MandateStatusUpdated =>
                    logger.info(s"[Payment Hooks] Mandate has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Mandate has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case EventType.MANDATE_ACTIVATED =>
                run(UpdateMandateStatus(ressourceId, Some(BankAccount.MandateStatus.MANDATE_ACTIVATED))) completeWith {
                  case _: MandateStatusUpdated =>
                    logger.info(s"[Payment Hooks] Mandate has been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                  case _ =>
                    logger.warn(s"[Payment Hooks] Mandate has not been updated for $ressourceId -> $eventType")
                    complete(HttpResponse(StatusCodes.OK))
                }
              case _ =>
                logger.error(s"[Payment Hooks] Event $eventType for $ressourceId is not supported")
                complete(HttpResponse(StatusCodes.BadRequest))
            }
          case None =>
            logger.error(s"[Payment Hooks] Event $eventType for $ressourceId is not supported")
            complete(HttpResponse(StatusCodes.BadRequest))
        }
      }
    }
  }
}

object MangoPayPaymentService {
  def apply(_system: ActorSystem[_]): MangoPayPaymentService = {
    new MangoPayPaymentService {
      override implicit def system: ActorSystem[_] = _system
    }
  }
}
