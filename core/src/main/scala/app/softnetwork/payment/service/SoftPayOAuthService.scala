package app.softnetwork.payment.service

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.{AuthenticationFailedRejection, RejectionHandler, Route}
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccessTokenRefreshed,
  AccountErrorMessage,
  GenerateAccessToken,
  Tokens
}
import app.softnetwork.account.service.OAuthService
import app.softnetwork.payment.handlers.SoftPayAccountTypeKey
import app.softnetwork.payment.message.AccountMessages.{GenerateClientToken, RefreshClientToken}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.config.Settings
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.SessionConfig
import org.json4s.Formats

trait SoftPayOAuthService[SD <: SessionData with SessionDataDecorator[SD]]
    extends OAuthService[SD]
    with SoftPayAccountTypeKey
    with ClientSessionDirectives[SD] {
  _: SessionMaterials[SD] =>

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

  override val route: Route = {
    pathPrefix(AccountSettings.OAuthPath) {
      concat(token ~ client :: (signin ++ callback).toList: _*)
    }
  }

  override lazy val token: Route =
    path("token") {
      post {
        formField("grant_type") {
          handlGrantType
        }
      }
    }

  protected def handlGrantType(grantType: String): Route = {
    grantType match {
      case "client_credentials" =>
        handleClientCredentialsGrantType
      case "refresh_token" =>
        handleRefreshTokenGrantType
      case _ => complete(StatusCodes.BadRequest)
    }
  }

  private def handleRefreshTokenGrantType: Route = {
    formField("refresh_token") { refreshToken =>
      run(refreshToken, RefreshClientToken(refreshToken)) completeWith {
        case r: AccessTokenRefreshed =>
          complete(
            StatusCodes.OK,
            Tokens(
              r.accessToken.token,
              r.accessToken.tokenType.toLowerCase(),
              r.accessToken.expiresIn,
              r.accessToken.refreshToken,
              r.accessToken.refreshExpiresIn
            )
          )
        case error: AccountErrorMessage =>
          complete(
            StatusCodes.BadRequest,
            Map(
              "error" -> "access_denied",
              "error_description" -> error.message
            )
          )
        case _ => complete(StatusCodes.BadRequest)
      }
    }
  }

  private def handleClientCredentialsGrantType: Route = {
    formField("credentials") { credentials =>
      val httpCredentials = BasicHttpCredentials(credentials)
      val clientId = httpCredentials.username
      val clientSecret = httpCredentials.password
      run(clientId, GenerateClientToken(clientId, clientSecret)) completeWith {
        case r: AccessTokenGenerated =>
          complete(
            StatusCodes.OK,
            Tokens(
              r.accessToken.token,
              r.accessToken.tokenType.toLowerCase(),
              r.accessToken.expiresIn,
              r.accessToken.refreshToken,
              r.accessToken.refreshExpiresIn
            )
          )
        case error: AccountErrorMessage =>
          complete(
            StatusCodes.BadRequest,
            Map(
              "error" -> "access_denied",
              "error_description" -> error.message
            )
          )
        case _ => complete(StatusCodes.BadRequest)
      }
    }
  }

  val pmClient: String = "me"

  lazy val client: Route = path(pmClient) {
    get {
      handleRejections(
        RejectionHandler
          .newBuilder()
          .handleAll[AuthenticationFailedRejection](authenticationFailedRejectionHandler)
          .result()
      ) {
        authenticateOAuth2Async(AccountSettings.Realm, oauthClient) { client =>
          setSession(sc(clientSessionManager(client)), st, client.asInstanceOf[SD]) {
            complete(StatusCodes.OK)
          }
        }
      }
    }
  }

}
