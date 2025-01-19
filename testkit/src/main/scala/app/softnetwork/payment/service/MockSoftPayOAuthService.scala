package app.softnetwork.payment.service

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.{AccessTokenGenerated, AccountErrorMessage, GenerateAccessToken, Tokens}
import app.softnetwork.account.spi.OAuth2Service
import app.softnetwork.payment.handlers.MockSoftPayAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.github.scribejava.core.oauth.DummyApiService

trait MockSoftPayOAuthService[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayOAuthService[SD]
    with MockSoftPayAccountTypeKey {
  _: SessionMaterials[SD] =>
  override lazy val services: Seq[OAuth2Service] =
    Seq(
      new DummyApiService()
    )

  override val pmClient: String = "client"

  override val route: Route = {
    pathPrefix(AccountSettings.OAuthPath) {
      concat(
        authorize ~ // for testing purposes
        token ~
        me ~ // for testing purposes
        client :: (signin ++ callback).toList: _*
      )
    }
  }

  override protected def handlGrantType(grantType: String): Route = {
    grantType match {
      case "authorization_code" =>
        handleAuthorizationCodeGrantType // for testing purposes
      case _ => super.handlGrantType(grantType)
    }
  }

  private def handleAuthorizationCodeGrantType: Route = {
    formFields("code", "redirect_uri".?, "client_id") { (code, redirectUri, clientId) =>
      run(code, GenerateAccessToken(clientId, code, redirectUri)) completeWith {
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

}
