package app.softnetwork.payment.service

import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccountErrorMessage,
  GenerateAccessToken,
  Tokens
}
import app.softnetwork.account.spi.OAuth2Service
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.handlers.MockSoftPayAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.github.scribejava.core.oauth.DummyApiService
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait MockSoftPayOAuthServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayOAuthServiceEndpoints[SD]
    with MockSoftPayAccountTypeKey { _: SessionMaterials[SD] =>

  override lazy val services: Seq[OAuth2Service] =
    Seq(
      new DummyApiService()
    )

  override val pmClient: String = "client"

  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      authorize, // for testing purposes
      token,
      me, // for testing purposes
      client
    ) ++ services.map(signin) ++ services.map(callback)

  override protected def handleGrantType(
    tokenRequest: ClientTokenRequest
  ): Future[Either[ApiErrors.BadRequest, Tokens]] = {
    tokenRequest match {
      case tokenRequest: AuthorizationCodeRequest =>
        handleAuthorizationCodeGrantType(tokenRequest) // for testing purposes
      case other =>
        super.handleGrantType(other)
    }
  }

  private def handleAuthorizationCodeGrantType(
    tokenRequest: AuthorizationCodeRequest
  ): Future[Either[ApiErrors.BadRequest, Tokens]] = {
    import tokenRequest._
    run(code, GenerateAccessToken(client_id, code, redirect_uri)) map {
      case r: AccessTokenGenerated =>
        Right(
          Tokens(
            r.accessToken.token,
            r.accessToken.tokenType.toLowerCase(),
            r.accessToken.expiresIn,
            r.accessToken.refreshToken,
            r.accessToken.refreshExpiresIn
          )
        )
      case error: AccountErrorMessage =>
        Left(ApiErrors.BadRequest(error.message))
      case _ => Left(ApiErrors.BadRequest("Unknown"))
    }
  }

}
