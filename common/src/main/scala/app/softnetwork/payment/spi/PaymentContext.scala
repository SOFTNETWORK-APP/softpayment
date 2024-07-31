package app.softnetwork.payment.spi

import app.softnetwork.payment.config.ProviderConfig
import app.softnetwork.payment.model.SoftPayAccount
import com.typesafe.scalalogging.Logger

trait PaymentContext {

  protected def mlog: Logger

  implicit def provider: SoftPayAccount.Client.Provider

  implicit def config: ProviderConfig
}
