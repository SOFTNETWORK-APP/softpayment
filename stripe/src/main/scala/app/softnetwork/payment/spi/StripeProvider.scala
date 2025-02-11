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
    with StripeBalanceApi {

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
    stripeConfig.softPayProvider
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
