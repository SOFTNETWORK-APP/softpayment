package app.softnetwork.payment.spi

import app.softnetwork.payment.model.SoftPaymentAccount

trait PaymentProviderSpi {
  def providerType: SoftPaymentAccount.Client.Provider.ProviderType

  def paymentProvider(p: SoftPaymentAccount.Client.Provider): PaymentProvider

  def softPaymentProvider: SoftPaymentAccount.Client.Provider
}
