package app.softnetwork.payment.config

import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import com.mangopay.MangoPayApi
import com.mangopay.core.enumerations.{EventType, HookStatus}
import com.mangopay.entities.Hook

import java.net.URL
import scala.util.{Failure, Success, Try}

/** Created by smanciot on 16/08/2018.
  */
object MangoPay {

  trait MangoPayConfig extends ProviderConfig {
    val technicalErrors: Set[String]
    override def withPaymentConfig(paymentConfig: Payment.Config): MangoPayConfig
  }

  case class Config(
    override val clientId: String,
    override val apiKey: String,
    override val baseUrl: String,
    override val version: String,
    override val debug: Boolean,
    technicalErrors: Set[String],
    override val secureModePath: String,
    override val hooksPath: String,
    override val mandatePath: String,
    override val paypalPath: String,
    paymentConfig: Payment.Config = PaymentSettings.PaymentConfig
  ) extends ProviderConfig(
        clientId,
        apiKey,
        baseUrl,
        version,
        debug,
        secureModePath,
        hooksPath,
        mandatePath,
        paypalPath
      )
      with MangoPayConfig {

    override def `type`: Provider.ProviderType = Provider.ProviderType.MANGOPAY

    override def withPaymentConfig(paymentConfig: Payment.Config): Config =
      this.copy(paymentConfig = paymentConfig)
  }

  var mangoPayApis: Map[String, MangoPayApi] = Map.empty

  lazy val softPayProvider: SoftPayAccount.Client.Provider =
    SoftPayAccount.Client.Provider.defaultInstance
      .withProviderType(SoftPayAccount.Client.Provider.ProviderType.MANGOPAY)
      .withProviderId(MangoPaySettings.MangoPayConfig.clientId)
      .withProviderApiKey(MangoPaySettings.MangoPayConfig.apiKey)

  def apply()(implicit
    provider: SoftPayAccount.Client.Provider,
    config: MangoPayConfig
  ): MangoPayApi = {
    mangoPayApis.get(provider.providerId) match {
      case Some(mangoPayApi) => mangoPayApi
      case _                 =>
        // init MangoPay api
        import MangoPaySettings.MangoPayConfig._
        val mangoPayApi = new MangoPayApi
        mangoPayApi.getConfig.setBaseUrl(baseUrl)
        mangoPayApi.getConfig.setClientId(provider.providerId)
        mangoPayApi.getConfig.setClientPassword(provider.providerApiKey)
        mangoPayApi.getConfig.setDebugMode(debug)
        val url = new URL(s"${config.hooksBaseUrl}")
        if (!Seq("localhost", "127.0.0.1").contains(url.getHost)) {
          // init MangoPay hooks
          import scala.collection.JavaConverters._
          val hooks: List[Hook] =
            Try(mangoPayApi.getHookApi.getAll) match {
              case Success(s) => s.asScala.toList
              case Failure(f) =>
                Console.err.println(f.getMessage, f.getCause)
                List.empty
            }
          createOrUpdateHook(mangoPayApi, EventType.KYC_SUCCEEDED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.KYC_FAILED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.KYC_OUTDATED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.TRANSFER_NORMAL_SUCCEEDED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.TRANSFER_NORMAL_FAILED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.UBO_DECLARATION_REFUSED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.UBO_DECLARATION_VALIDATED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.UBO_DECLARATION_INCOMPLETE, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.USER_KYC_REGULAR, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.USER_KYC_LIGHT, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.MANDATE_FAILED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.MANDATE_SUBMITTED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.MANDATE_CREATED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.MANDATE_ACTIVATED, hooks, config)
          createOrUpdateHook(mangoPayApi, EventType.MANDATE_EXPIRED, hooks, config)
        }
        mangoPayApis = mangoPayApis.updated(provider.providerId, mangoPayApi)
        mangoPayApi
    }
  }

  private[payment] def createOrUpdateHook(
    mangoPayApi: MangoPayApi,
    eventType: EventType,
    hooks: List[Hook],
    config: MangoPayConfig
  ): Unit = {
    Try {
      hooks.find(_.getEventType == eventType) match {
        case Some(previousHook) =>
          previousHook.setStatus(HookStatus.ENABLED)
          previousHook.setUrl(s"${config.hooksBaseUrl}")
          Console.println(s"Updating Mangopay Hook ${previousHook.getId}")
          mangoPayApi.getHookApi.update(previousHook)
        case _ =>
          val hook = new Hook()
          hook.setEventType(eventType)
          hook.setStatus(HookStatus.ENABLED)
          hook.setUrl(s"${config.hooksBaseUrl}")
          mangoPayApi.getHookApi.create(hook)
      }
    } match {
      case Success(_) =>
      case Failure(f) =>
        Console.err.println(s"$eventType -> ${f.getMessage}", f.getCause)
    }
  }
}
