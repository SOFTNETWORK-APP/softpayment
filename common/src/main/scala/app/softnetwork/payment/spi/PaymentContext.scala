package app.softnetwork.payment.spi

import app.softnetwork.payment.config.ProviderConfig
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.serialization.paymentFormats
import com.typesafe.scalalogging.Logger
import org.json4s.Formats

trait PaymentContext {

  protected def mlog: Logger

  implicit def provider: SoftPayAccount.Client.Provider

  implicit def config: ProviderConfig

  implicit def formats: Formats = paymentFormats
}
