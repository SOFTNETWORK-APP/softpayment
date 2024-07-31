package app.softnetwork.payment.api

import app.softnetwork.payment.config.{MangoPay, MangoPaySettings, ProviderConfig}
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.spi.MockMangoPayConfig
import app.softnetwork.persistence.scalatest.PersistenceTestKit
import app.softnetwork.security.sha256

trait PaymentProviderTestKit { _: PersistenceTestKit =>

  implicit lazy val provider: SoftPayAccount.Client.Provider = providerConfig.softPayProvider

  implicit lazy val providerConfig: ProviderConfig = MockMangoPayConfig(
    MangoPaySettings.MangoPayConfig
  )

  def providerSettings: String =
    s"""
       |payment.test = true
       |payment.client-id = "${provider.clientId}"
       |payment.api-key = "${sha256(provider.providerApiKey)}"
       |""".stripMargin
}
