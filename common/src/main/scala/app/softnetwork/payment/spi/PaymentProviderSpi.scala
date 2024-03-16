package app.softnetwork.payment.spi

import app.softnetwork.payment.model.SoftPayAccount

trait PaymentProviderSpi {
  def providerType: SoftPayAccount.Client.Provider.ProviderType

  def paymentProvider(p: SoftPayAccount.Client.Provider): PaymentProvider

  def softPaymentProvider: SoftPayAccount.Client.Provider
}
