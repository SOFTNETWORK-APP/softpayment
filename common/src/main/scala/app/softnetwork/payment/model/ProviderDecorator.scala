package app.softnetwork.payment.model

import app.softnetwork.account.model.AccountStatus
import app.softnetwork.payment.spi.PaymentProviders
import app.softnetwork.security.sha256

trait ProviderDecorator { self: SoftPaymentAccount.Client.Provider =>
  lazy val clientId = s"$providerId.${providerType.name.toLowerCase}"

  lazy val client: SoftPaymentAccount.Client = {
    PaymentProviders.paymentProvider(self).client match {
      case Some(client) =>
        client.withClientApiKey(sha256(self.providerApiKey))
      case _ =>
        throw new Exception(s"PaymentProvider not found for providerType: $providerType")
    }
  }

  lazy val account: SoftPaymentAccount = {
    SoftPaymentAccount.defaultInstance
      .withUuid(clientId)
      .withAnonymous(true)
      .withClients(Seq(client))
      .withStatus(AccountStatus.Active)
  }
}
