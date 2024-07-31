package app.softnetwork.payment.spi

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.service.{HooksDirectives, HooksEndpoints}
import com.typesafe.config.Config
import org.json4s.Formats

trait PaymentProviderSpi {
  def providerType: SoftPayAccount.Client.Provider.ProviderType

  def paymentProvider(p: SoftPayAccount.Client.Provider): PaymentProvider

  def softPaymentProvider(config: Config): SoftPayAccount.Client.Provider

  def hooksPath: String = providerType.name.toLowerCase

  def hooksDirectives(implicit system: ActorSystem[_], formats: Formats): HooksDirectives

  def hooksEndpoints(implicit system: ActorSystem[_], formats: Formats): HooksEndpoints
}
