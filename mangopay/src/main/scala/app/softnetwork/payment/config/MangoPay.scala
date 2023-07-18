package app.softnetwork.payment.config

import MangoPaySettings._
import com.mangopay.MangoPayApi
import com.mangopay.core.enumerations.{EventType, HookStatus}
import com.mangopay.entities.Hook

import com.typesafe.scalalogging.StrictLogging

import scala.util.{Failure, Success, Try}

/** Created by smanciot on 16/08/2018.
  */
object MangoPay extends StrictLogging {

  case class Config(
    clientId: String,
    apiKey: String,
    baseUrl: String,
    version: String,
    debug: Boolean,
    technicalErrors: Set[String],
    secureModePath: String,
    hooksPath: String,
    mandatePath: String,
    paypalPath: String
  ) {

    lazy val secureModeReturnUrl = s"""$BaseUrl/$secureModePath/$SecureModeRoute"""

    lazy val preAuthorizeCardFor3DS = s"$secureModeReturnUrl/$PreAuthorizeCardRoute"

    lazy val payInFor3DS = s"$secureModeReturnUrl/$PayInRoute"

    lazy val recurringPaymentFor3DS = s"$secureModeReturnUrl/$RecurringPaymentRoute"

    lazy val hooksBaseUrl = s"""$BaseUrl/$hooksPath/$HooksRoute"""

    lazy val mandateReturnUrl = s"""$BaseUrl/$mandatePath/$MandateRoute"""

    lazy val payPalReturnUrl = s"""$BaseUrl/$paypalPath/$PayPalRoute"""
  }

  var maybeMangoPayApi: Option[MangoPayApi] = None

  def apply(): MangoPayApi = {
    maybeMangoPayApi match {
      case Some(mangoPayApi) => mangoPayApi
      case _                 =>
        // init MangoPay api
        import MangoPaySettings.MangoPayConfig._
        val mangoPayApi = new MangoPayApi
        mangoPayApi.getConfig.setBaseUrl(baseUrl)
        mangoPayApi.getConfig.setClientId(clientId)
        mangoPayApi.getConfig.setClientPassword(apiKey)
        mangoPayApi.getConfig.setDebugMode(debug)
        // init MangoPay hooks
        import scala.collection.JavaConverters._
        val hooks: List[Hook] =
          Try(mangoPayApi.getHookApi.getAll) match {
            case Success(s) => s.asScala.toList
            case Failure(f) =>
              logger.error(f.getMessage, f.getCause)
              List.empty
          }
        createOrUpdateHook(mangoPayApi, EventType.KYC_SUCCEEDED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.KYC_FAILED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.KYC_OUTDATED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.TRANSFER_NORMAL_SUCCEEDED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.TRANSFER_NORMAL_FAILED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.UBO_DECLARATION_REFUSED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.UBO_DECLARATION_VALIDATED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.UBO_DECLARATION_INCOMPLETE, hooks)
        createOrUpdateHook(mangoPayApi, EventType.USER_KYC_REGULAR, hooks)
        createOrUpdateHook(mangoPayApi, EventType.USER_KYC_LIGHT, hooks)
        createOrUpdateHook(mangoPayApi, EventType.MANDATE_FAILED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.MANDATE_SUBMITTED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.MANDATE_CREATED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.MANDATE_ACTIVATED, hooks)
        createOrUpdateHook(mangoPayApi, EventType.MANDATE_EXPIRED, hooks)
        maybeMangoPayApi = Some(mangoPayApi)
        mangoPayApi
    }
  }

  private[payment] def createOrUpdateHook(
    mangoPayApi: MangoPayApi,
    eventType: EventType,
    hooks: List[Hook]
  ): Unit = {
    import MangoPaySettings.MangoPayConfig._
    Try {
      hooks.find(_.getEventType == eventType) match {
        case Some(previousHook) =>
          previousHook.setStatus(HookStatus.ENABLED)
          previousHook.setUrl(s"$hooksBaseUrl")
          logger.info(s"Updating Mangopay Hook ${previousHook.getId}")
          mangoPayApi.getHookApi.update(previousHook)
        case _ =>
          val hook = new Hook()
          hook.setEventType(eventType)
          hook.setStatus(HookStatus.ENABLED)
          hook.setUrl(s"$hooksBaseUrl")
          mangoPayApi.getHookApi.create(hook)
      }
    } match {
      case Success(_) =>
      case Failure(f) =>
        logger.error(s"$eventType -> ${f.getMessage}", f.getCause)
    }
  }
}
