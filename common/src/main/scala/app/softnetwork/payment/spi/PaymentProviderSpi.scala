package app.softnetwork.payment.spi

import app.softnetwork.payment.model.SoftPayAccount

trait PaymentProviderSpi {
  def providerType: SoftPayAccount.SoftPayClient.SoftPayProvider.SoftPayProviderType

  def paymentProvider(p: SoftPayAccount.SoftPayClient.SoftPayProvider): PaymentProvider

  def softPaymentProvider: SoftPayAccount.SoftPayClient.SoftPayProvider
}
