package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPayAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait MockSoftPayAccountServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayAccountServiceEndpoints[SD]
    with MockSoftPayAccountTypeKey { _: SessionMaterials[SD] =>
  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] = {
    List(
      anonymous, // for testing purposes
      signUp,
      principal, // for testing purposes
      basic, // for testing purposes
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
}
