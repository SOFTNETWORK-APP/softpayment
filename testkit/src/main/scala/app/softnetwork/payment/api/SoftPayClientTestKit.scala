package app.softnetwork.payment.api

import app.softnetwork.payment.config.MangoPay
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.security.sha256

trait SoftPayClientTestKit {

  def provider: SoftPaymentAccount.Client.Provider =
    MangoPay.softPaymentProvider.withProviderType(
      SoftPaymentAccount.Client.Provider.ProviderType.MOCK
    )

  def softPayClientSettings: String =
    s"""
       |payment.test = true
       |payment.client-id = "${provider.clientId}"
       |payment.api-key = "${sha256(provider.providerApiKey)}"
       |""".stripMargin
}
