package app.softnetwork.payment.cli.signup

import app.softnetwork.payment.api.ProviderType

case class SignUpClientConfig(
  principal: String = "",
  credentials: Array[Char] = Array.emptyCharArray,
  providerId: String = "",
  providerApiKey: Array[Char] = Array.emptyCharArray,
  providerType: Option[ProviderType] = Some(ProviderType.MANGOPAY)
)
