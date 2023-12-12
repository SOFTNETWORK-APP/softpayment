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
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.persistence.message.EntityCommand
import org.softnetwork.session.model.ApiKey

object AccountMessages {
  case class SoftPaySignUp(
    login: String,
    password: String,
    provider: SoftPayAccount.SoftPayClient.SoftPayProvider,
    override val confirmPassword: Option[String] = None,
    override val profile: Option[BasicAccountProfile] = None
  ) extends SignUp

  case class RegisterProvider(provider: SoftPayAccount.SoftPayClient.SoftPayProvider)
      extends AccountCommand

  case class RegisterAccountWithProvider(provider: SoftPayAccount.SoftPayClient.SoftPayProvider)
      extends AccountCommand
      with EntityCommand {
    override def id: String = provider.clientId
  }

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

  case class ProviderRegistered(client: SoftPayAccount.SoftPayClient) extends AccountCommandResult

  case class AccountWithProviderRegistered(account: SoftPayAccount) extends AccountCommandResult

  case class ClientLoaded(client: SoftPayAccount.SoftPayClient) extends AccountCommandResult

  case class ApiKeysLoaded(apiKeys: Seq[ApiKey]) extends AccountCommandResult

  case class ApiKeyLoaded(apiKey: ApiKey) extends AccountCommandResult

  case class OAuthClientSucceededResult(client: SoftPayAccount.SoftPayClient)
      extends AccountCommandResult

  case object ProviderAlreadyRegistered extends AccountErrorMessage("provider.already.registered")

  case object ProviderNotRegistered extends AccountErrorMessage("provider.not.registered")

  case object AccountWithProviderNotRegistered
      extends AccountErrorMessage("account.with.provider.not.registered")

  case object ClientNotFound extends AccountErrorMessage("client.not.found")

  case object ApiKeyNotFound extends AccountErrorMessage("api.key.not.found")

  case class InactiveAccount(help: String) extends AccountErrorMessage(s"InactiveAccount\n$help")
}
