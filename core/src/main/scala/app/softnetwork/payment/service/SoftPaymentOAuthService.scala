package app.softnetwork.payment.service

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.{AuthenticationFailedRejection, RejectionHandler, Route}
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccessTokenRefreshed,
  AccountErrorMessage,
  Tokens
}
import app.softnetwork.account.service.OAuthService
import app.softnetwork.payment.config.PaymentSettings.ClientSessionConfig
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.payment.message.AccountMessages.{GenerateClientToken, RefreshClientToken}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.SessionConfig
import org.json4s.Formats
import org.softnetwork.session.model.Session

trait SoftPaymentOAuthService
    extends OAuthService
    with SoftPaymentAccountTypeKey
    with ClientSessionDirectives {
  _: SessionMaterials =>

  implicit def sessionConfig: SessionConfig = ClientSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

  override val route: Route = {
    pathPrefix(AccountSettings.OAuthPath) {
      concat(token ~ me :: (signin ++ backup).toList: _*)
    }
  }

  override lazy val token: Route =
    path("token") {
      post {
        formField("grant_type") {
          case "client_credentials" =>
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
                      AccountSettings.OAuthSettings.accessToken.expirationTime * 60,
                      r.accessToken.refreshToken
                    )
                  )
                case error: AccountErrorMessage =>
                  complete(
                    StatusCodes.BadRequest,
                    Map(
                      "error"             -> "access_denied",
                      "error_description" -> error.message
                    )
                  )
                case _ => complete(StatusCodes.BadRequest)
              }
            }
          case "refresh_token" =>
            formField("refresh_token") { refreshToken =>
              run(refreshToken, RefreshClientToken(refreshToken)) completeWith {
                case r: AccessTokenRefreshed =>
                  complete(
                    StatusCodes.OK,
                    Tokens(
                      r.accessToken.token,
                      r.accessToken.tokenType.toLowerCase(),
                      AccountSettings.OAuthSettings.accessToken.expirationTime * 60,
                      r.accessToken.refreshToken
                    )
                  )
                case error: AccountErrorMessage =>
                  complete(
                    StatusCodes.BadRequest,
                    Map(
                      "error"             -> "access_denied",
                      "error_description" -> error.message
                    )
                  )
                case _ => complete(StatusCodes.BadRequest)
              }
            }
          case _ => complete(StatusCodes.BadRequest)
        }
      }
    }

  override lazy val me: Route = path("me") {
    get {
      handleRejections(
        RejectionHandler
          .newBuilder()
          .handleAll[AuthenticationFailedRejection](authenticationFailedRejectionHandler)
          .result()
      ) {
        authenticateOAuth2Async(AccountSettings.Realm, oauthClient) { client =>
          setSession(sc(clientSessionManager(client.clientId)), st, client.asInstanceOf[Session]) {
            complete(StatusCodes.OK)
          }
        }
      }
    }
  }

}
