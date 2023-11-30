package app.softnetwork.payment.model

trait ProviderDecorator { _: SoftPaymentAccount.Client.Provider =>
  lazy val clientId = s"$providerId.${providerType.name.toLowerCase}"
}
