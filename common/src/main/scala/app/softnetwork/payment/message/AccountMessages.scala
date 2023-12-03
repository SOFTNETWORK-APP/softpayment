package app.softnetwork.payment.message

import app.softnetwork.account.message.{
  AccountCommand,
  AccountCommandResult,
  AccountErrorMessage,
  LookupAccountCommand,
  SignUp
}
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.model.SoftPaymentAccount
import org.softnetwork.session.model.ApiKey

object AccountMessages {
  case class SoftPaymentSignup(
    login: String,
    password: String,
    provider: SoftPaymentAccount.Client.Provider,
    override val confirmPassword: Option[String] = None,
    override val profile: Option[BasicAccountProfile] = None
  ) extends SignUp

  case class RegisterProvider(provider: SoftPaymentAccount.Client.Provider) extends AccountCommand

  @InternalApi
  private[payment] case class LoadClient(clientId: String) extends LookupAccountCommand

  case class LoadApiKey(clientId: String) extends LookupAccountCommand

  case object ListApiKeys extends AccountCommand

  case class GenerateClientToken(
    client_id: String,
    client_secret: String,
    scope: Option[String] = None
  ) extends LookupAccountCommand

  case class RefreshClientToken(refreshToken: String) extends LookupAccountCommand

  case class OAuthClient(token: String) extends LookupAccountCommand

  case class ProviderRegistered(client: SoftPaymentAccount.Client) extends AccountCommandResult

  case class ClientLoaded(client: SoftPaymentAccount.Client) extends AccountCommandResult

  case class ApiKeysLoaded(apiKeys: Seq[ApiKey]) extends AccountCommandResult

  case class ApiKeyLoaded(apiKey: ApiKey) extends AccountCommandResult

  case class OAuthClientSucceededResult(client: SoftPaymentAccount.Client)
      extends AccountCommandResult

  case object ProviderAlreadyRegistered extends AccountErrorMessage("provider.already.registered")

  case object ProviderNotRegistered extends AccountErrorMessage("provider.not.registered")

  case object ClientNotFound extends AccountErrorMessage("client.not.found")

  case object ApiKeyNotFound extends AccountErrorMessage("api.key.not.found")
}
