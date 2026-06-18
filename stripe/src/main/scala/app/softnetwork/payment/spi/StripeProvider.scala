package app.softnetwork.payment.spi

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.config.{StripeApi, StripeSettings}
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.model.SoftPayAccount.Client
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.payment.service.{
  HooksDirectives,
  HooksEndpoints,
  StripeHooksDirectives,
  StripeHooksEndpoints
}
import com.typesafe.config.Config
import org.json4s.Formats
import org.slf4j.Logger

import scala.util.{Failure, Try}

trait StripeContext extends PaymentContext {

  override implicit def config: StripeApi.Config

}

trait StripeProvider
    extends PaymentProvider
    with StripeContext
    with StripeAccountApi
    with StripeDirectDebitApi
    with StripePreAuthorizationApi
    with StripePaymentMethodApi
    with StripePayInApi
    with StripePayOutApi
    with StripeRecurringPaymentApi
    with StripeRefundApi
    with StripeTransferApi
    with StripeBalanceApi
    with StripeBillingPortalApi {

  /** @return
    *   client
    */
  override def client: Option[Client] = Some(
    SoftPayAccount.Client.defaultInstance
      .withProvider(provider)
      .withClientId(provider.clientId)
  )

}

class StripeProviderFactory extends PaymentProviderSpi {

  @volatile private[this] var _config: Option[StripeApi.Config] = None

  private[this] lazy val log: Logger = org.slf4j.LoggerFactory.getLogger(getClass)

  override def providerType: Provider.ProviderType = Provider.ProviderType.STRIPE

  override def paymentProvider(p: Client.Provider): StripeProvider = {
    new StripeProvider {
      override implicit def provider: Provider = p
      override implicit def config: StripeApi.Config = _config
        .getOrElse(StripeSettings.StripeApiConfig)
    }
  }

  override def softPaymentProvider(config: Config): Client.Provider = {
    val stripeConfig = StripeSettings(config).StripeApiConfig
    _config = Some(stripeConfig)
    val provider = stripeConfig.softPayProvider
    // Eagerly initialize the Stripe API at application startup so the platform webhook endpoint
    // (and its signing secret) is provisioned up-front — before any payment operation or inbound
    // webhook event — instead of lazily on first use. Guarded so a transient Stripe error does not
    // prevent the application from starting; StripeApi() is retried on first lazy use.
    Try(StripeApi()(provider, stripeConfig)) match {
      case Failure(f) =>
        log.warn(s"Failed to initialize Stripe API at startup: ${f.getMessage}")
      case _ =>
    }
    provider
  }

  override def hooksDirectives(implicit
    _system: ActorSystem[_],
    _formats: Formats
  ): HooksDirectives = {
    new StripeHooksDirectives {
      override def log: Logger = org.slf4j.LoggerFactory.getLogger(getClass)

      override implicit def formats: Formats = _formats

      override implicit def system: ActorSystem[_] = _system
    }
  }

  override def hooksEndpoints(implicit
    _system: ActorSystem[_],
    formats: Formats
  ): HooksEndpoints = {
    new StripeHooksEndpoints {
      override def log: Logger = org.slf4j.LoggerFactory.getLogger(getClass)

      override implicit def system: ActorSystem[_] = _system
    }
  }
}
