package app.softnetwork.payment.spi

import app.softnetwork.payment.model.SoftPaymentAccount

import java.util.ServiceLoader
import scala.collection.JavaConverters._

object PaymentProviders {

  private[this] lazy val paymentProviderFactories: ServiceLoader[PaymentProviderSpi] =
    ServiceLoader.load(classOf[PaymentProviderSpi])

  private[this] var paymentProviders: Map[String, PaymentProvider] = Map.empty

  private[this] def paymentProviderKey(provider: SoftPaymentAccount.Client.Provider) =
    s"${provider.providerType}-${provider.providerId}"

  def paymentProvider(
    provider: SoftPaymentAccount.Client.Provider
  ): PaymentProvider = {
    paymentProviders.get(paymentProviderKey(provider)) match {
      case Some(paymentProvider) => paymentProvider
      case _ =>
        val paymentProvider =
          paymentProviderFactories
            .iterator()
            .asScala
            .find(_.providerType == provider.providerType)
            .map(_.paymentProvider(provider))
            .getOrElse(
              throw new Exception(
                s"PaymentProvider not found for providerType: ${provider.providerType}"
              )
            )
        paymentProviders += paymentProviderKey(provider) -> paymentProvider
        paymentProvider
    }
  }

}
