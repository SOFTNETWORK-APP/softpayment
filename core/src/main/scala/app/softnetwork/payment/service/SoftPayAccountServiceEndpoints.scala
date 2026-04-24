package app.softnetwork.payment.service

import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.{LoadProfile, ProfileLoaded}
import app.softnetwork.account.model.{DefaultProfileView, ProfileType}
import app.softnetwork.account.service.BasicAccountServiceEndpoints
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.handlers.SoftPayAccountTypeKey
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.config.Settings
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.SessionConfig
import org.json4s.Formats
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{query, Schema}

import scala.concurrent.Future

trait SoftPayAccountServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends BasicAccountServiceEndpoints[SD]
    with SoftPayAccountTypeKey { _: SessionMaterials[SD] =>

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

  import app.softnetwork.serialization.serialization

  implicit lazy val profileTypeSchema: Schema[ProfileType] = Schema.derived
  implicit lazy val defaultProfileViewSchema: Schema[DefaultProfileView] = Schema.derived

  lazy val loadProfile: ServerEndpoint[Any with AkkaStreams, Future] =
    ApiErrors
      .withApiErrorVariants(
        antiCsrfWithRequiredSession(sc, gt, checkMode)
      )
      .in(AccountSettings.Path / "profile")
      .in(query[Option[String]]("name"))
      .get
      .out(jsonBody[DefaultProfileView])
      .serverLogic(session =>
        name =>
          run(session.id, LoadProfile(session.id, name)).map {
            case r: ProfileLoaded =>
              Right(r.profile.view.asInstanceOf[DefaultProfileView])
            case other => Left(resultToApiError(other))
          }
      )

  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      loadProfile,
      signUp,
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
