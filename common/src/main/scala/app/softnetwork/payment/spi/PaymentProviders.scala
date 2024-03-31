package app.softnetwork.payment.spi

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.service.{HooksDirectives, HooksEndpoints}
import org.json4s.Formats

import java.util.ServiceLoader
import scala.collection.JavaConverters._

object PaymentProviders {

  private[this] lazy val paymentProviderFactories: ServiceLoader[PaymentProviderSpi] =
    ServiceLoader.load(classOf[PaymentProviderSpi])

  private[this] var paymentProviders: Map[String, PaymentProvider] = Map.empty

  def defaultPaymentProviders: Seq[SoftPayAccount.Client.Provider] =
    paymentProviderFactories.iterator().asScala.map(_.softPaymentProvider).toSeq

  def hooksDirectives(implicit
    system: ActorSystem[_],
    formats: Formats
  ): Map[String, HooksDirectives] = {
    paymentProviderFactories
      .iterator()
      .asScala
      .map { factory =>
        factory.hooksPath -> factory.hooksDirectives
      }
      .toMap
  }

  def hooksEndpoints(implicit
    system: ActorSystem[_],
    formats: Formats
  ): Map[String, HooksEndpoints] = {
    paymentProviderFactories
      .iterator()
      .asScala
      .map { factory =>
        factory.hooksPath -> factory.hooksEndpoints
      }
      .toMap
  }

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
