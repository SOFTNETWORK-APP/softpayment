package app.softnetwork.payment.service

import app.softnetwork.account.service.BasicAccountServiceEndpoints
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.config.Settings
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.SessionConfig
import org.json4s.Formats
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait SoftPaymentAccountServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends BasicAccountServiceEndpoints[SD]
    with SoftPaymentAccountTypeKey { _: SessionMaterials[SD] =>

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

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
