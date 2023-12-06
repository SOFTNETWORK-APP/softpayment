package app.softnetwork.payment.api

import app.softnetwork.payment.api.config.PaymentClientSettings
import app.softnetwork.payment.config.MangoPay
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.security.sha256

trait PaymentClientTestKit {

  def provider: SoftPaymentAccount.Client.Provider =
    MangoPay.softPaymentProvider.withProviderType(
      SoftPaymentAccount.Client.Provider.ProviderType.MOCK
    )

  def settings: PaymentClientSettings =
    PaymentClientSettings(provider.clientId, sha256(provider.providerApiKey))
}
