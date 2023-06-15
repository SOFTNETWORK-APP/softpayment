package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.MangoPayPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{BankAccount, KycDocument, UboDeclaration}
import app.softnetwork.session.service.SessionEndpoints
import com.mangopay.core.enumerations.EventType
import org.slf4j.{Logger, LoggerFactory}
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir._

import scala.concurrent.Future

trait MangoPayPaymentEndpoints extends GenericPaymentEndpoints with MangoPayPaymentHandler {

  /** should be implemented by each payment provider
    */
  override lazy val hooks: Full[Unit, Unit, (String, String), Unit, Unit, Any, Future] =
    rootEndpoint
      .in(PaymentSettings.HooksRoute)
      .in(query[String]("EventType"))
      .in(query[String]("RessourceId"))
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] KYC has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] KYC has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] KYC has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Ubo Declaration has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Ubo Declaration has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Ubo Declaration has not been updated for $resourceId -> $eventType"
                    )
                    Right()
                }
              case EventType.USER_KYC_REGULAR =>
                run(ValidateRegularUser(resourceId)).map {
                  case RegularUserValidated =>
                    log.info(
                      s"[Payment Hooks] Regular User has been validated for $resourceId -> $eventType"
                    )
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Regular User has not been validated for $resourceId -> $eventType"
                    )
                    Right()
                }
              case EventType.USER_KYC_LIGHT =>
                run(InvalidateRegularUser(resourceId)).map {
                  case RegularUserValidated =>
                    log.info(
                      s"[Payment Hooks] Regular User has been validated for $resourceId -> $eventType"
                    )
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Regular User has not been validated for $resourceId -> $eventType"
                    )
                    Right()
                }
              case EventType.MANDATE_FAILED =>
                run(
                  UpdateMandateStatus(resourceId, Some(BankAccount.MandateStatus.MANDATE_FAILED))
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right()
                }
              case EventType.MANDATE_CREATED =>
                run(
                  UpdateMandateStatus(resourceId, Some(BankAccount.MandateStatus.MANDATE_CREATED))
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right()
                }
              case EventType.MANDATE_EXPIRED =>
                run(
                  UpdateMandateStatus(resourceId, Some(BankAccount.MandateStatus.MANDATE_EXPIRED))
                ).map {
                  case _: MandateStatusUpdated =>
                    log.info(
                      s"[Payment Hooks] Mandate has been updated for $resourceId -> $eventType"
                    )
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right()
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
                    Right()
                  case _ =>
                    log.warn(
                      s"[Payment Hooks] Mandate has not been updated for $resourceId -> $eventType"
                    )
                    Right()
                }
              case _ =>
                log.error(s"[Payment Hooks] Event $eventType for $resourceId is not supported")
                Future.successful(Left())
            }
          case None =>
            log.error(s"[Payment Hooks] Event $eventType for $resourceId is not supported")
            Future.successful(Left())
        }
      }

}

object MangoPayPaymentEndpoints {
  def apply(
    _system: ActorSystem[_],
    _sessionEndpoints: SessionEndpoints
  ): MangoPayPaymentEndpoints = {
    new MangoPayPaymentEndpoints {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = _system
      override def sessionEndpoints: SessionEndpoints = _sessionEndpoints
    }
  }
}
