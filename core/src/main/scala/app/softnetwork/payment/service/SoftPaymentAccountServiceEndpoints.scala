package app.softnetwork.payment.service

import app.softnetwork.account.service.BasicAccountServiceEndpoints
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait SoftPaymentAccountServiceEndpoints
    extends BasicAccountServiceEndpoints
    with SoftPaymentAccountTypeKey { _: SessionMaterials =>

  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      signUp,
      basic,
      login,
      signIn,
      activate,
      logout,
      signOut,
      sendVerificationCode,
      sendResetPasswordToken,
      checkResetPasswordToken,
      resetPassword,
      unsubscribe,
      registerDevice,
      unregisterDevice,
      updatePassword
    )
}
