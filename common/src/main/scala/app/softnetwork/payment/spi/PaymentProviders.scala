package app.softnetwork.payment.spi

import app.softnetwork.payment.model.SoftPayAccount

import java.util.ServiceLoader
import scala.collection.JavaConverters._

object PaymentProviders {

  private[this] lazy val paymentProviderFactories: ServiceLoader[PaymentProviderSpi] =
    ServiceLoader.load(classOf[PaymentProviderSpi])

  private[this] var paymentProviders: Map[String, PaymentProvider] = Map.empty

  def defaultPaymentProviders: Seq[SoftPayAccount.Client.Provider] =
    paymentProviderFactories.iterator().asScala.map(_.softPaymentProvider).toSeq

  def paymentProvider(
    provider: SoftPayAccount.Client.Provider
  ): PaymentProvider = {
    paymentProviders.get(provider.clientId) match {
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
        paymentProviders += provider.clientId -> paymentProvider
        paymentProvider
    }
  }

}
