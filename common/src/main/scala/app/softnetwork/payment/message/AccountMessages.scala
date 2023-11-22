package app.softnetwork.payment.message

import app.softnetwork.account.message.{
  AccountCommand,
  AccountCommandResult,
  AccountErrorMessage,
  SignUp
}
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.model.SoftPaymentAccount

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
  private[payment] case class LoadProvider(clientId: String) extends AccountCommand

  case class ProviderRegistered(client: SoftPaymentAccount.Client) extends AccountCommandResult

  case class ProviderLoaded(provider: SoftPaymentAccount.Client.Provider)
      extends AccountCommandResult

  case object ProviderAlreadyRegistered extends AccountErrorMessage("provider.already.registered")

  case object ProviderNotRegistered extends AccountErrorMessage("provider.not.registered")

  case object ProviderNotFound extends AccountErrorMessage("provider.not.found")
}
