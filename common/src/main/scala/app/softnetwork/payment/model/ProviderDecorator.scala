package app.softnetwork.payment.model

import app.softnetwork.account.model.{Principal, PrincipalType}
import app.softnetwork.payment.spi.PaymentProviders

trait ProviderDecorator { self: SoftPaymentAccount.Client.Provider =>
  lazy val clientId = s"$providerId.${providerType.name.toLowerCase}"

  lazy val client: SoftPaymentAccount.Client = {
    PaymentProviders.paymentProvider(self).client match {
      case Some(value) => value.withClientApiKey(self.providerApiKey)
      case _ =>
        throw new Exception(s"PaymentProvider not found for providerType: ${providerType}")
    }
  }

  lazy val account: SoftPaymentAccount = {
    SoftPaymentAccount.defaultInstance
      .withUuid(clientId)
      .withAnonymous(true)
      .withClients(Seq(client))
  }
}
