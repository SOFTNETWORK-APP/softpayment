package app.softnetwork.payment.service

import akka.http.scaladsl.model.headers.BasicHttpCredentials
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccessTokenRefreshed,
  AccountErrorMessage,
  BearerAuthenticationFailed,
  GenerateAccessToken,
  Tokens
}
import app.softnetwork.account.service.OAuthServiceEndpoints
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.handlers.SoftPayAccountTypeKey
import app.softnetwork.payment.message.AccountMessages.{
  GenerateClientToken,
  OAuthClient,
  OAuthClientSucceededResult,
  RefreshClientToken
}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.config.Settings
import app.softnetwork.session.httpCookieToTapirCookieWithMeta
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{CookieST, HeaderST, SessionConfig}
import org.json4s.Formats
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir.EndpointIO.Example
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait SoftPayOAuthServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends OAuthServiceEndpoints[SD]
    with SoftPayAccountTypeKey
    with ClientSession[SD] { _: SessionMaterials[SD] =>

  import app.softnetwork.serialization.serialization

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      token,
      client
    ) ++ services.map(signin) ++ services.map(callback)

  override val token: ServerEndpoint[Any with AkkaStreams, Future] =
    endpoint.post
      .in(AccountSettings.OAuthPath / "token")
      .description("OAuth2 token endpoint")
      .errorOut(ApiErrors.oneOfApiErrors)
      .in(
        formBody[Map[String, String]]
          .description("form body")
          .map[ClientTokenRequest](m => ClientTokenRequest.decode(m))(ClientTokenRequest.encode)
          .example(
            Example.of(
              ClientCredentials(
                "SplxlOBeZQQYbYS6WxSbIA",
                Some("*")
              ),
              Some("client credentials request")
            )
          )
          .example(
            Example.of(
              RefreshToken("tGzv3JOkF0XG5Qx2TlKWIA"),
              Some("refresh token request")
            )
          )
      )
      .out(jsonBody[Tokens])
      .serverLogic { case tokenRequest: ClientTokenRequest =>
        handleGrantType(tokenRequest)
      }

  protected def handleGrantType(
    tokenRequest: ClientTokenRequest
  ): Future[Either[ApiErrors.BadRequest, Tokens]] = {
    tokenRequest match {
      case tokenRequest: ClientCredentials =>
        handleClientCredentialsGrantType(tokenRequest)
      case tokenRequest: RefreshToken =>
        handleRefreshTokenGrantType(tokenRequest)
      case tokenRequest: UnsupportedGrantType =>
        Future.successful(
          Left(ApiErrors.BadRequest(s"Unknown grant_type ${tokenRequest.grantType}"))
        )
    }
  }

  private def handleRefreshTokenGrantType(
    tokenRequest: RefreshToken
  ): Future[Either[ApiErrors.BadRequest, Tokens]] = {
    run(tokenRequest.refreshToken, RefreshClientToken(tokenRequest.refreshToken)) map {
      case r: AccessTokenRefreshed =>
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

  private def handleClientCredentialsGrantType(
    tokenRequest: ClientCredentials
  ): Future[Either[ApiErrors.BadRequest, Tokens]] = {
    val httpCredentials = BasicHttpCredentials(tokenRequest.credentials)
    val clientId = httpCredentials.username
    val clientSecret = httpCredentials.password
    run(clientId, GenerateClientToken(clientId, clientSecret)) map {
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

  val pmClient: String = "me"

  lazy val client: ServerEndpoint[Any with AkkaStreams, Future] =
    endpoint.get
      .in(AccountSettings.OAuthPath / pmClient)
      .description("OAuth2 client endpoint")
      .securityIn(auth.bearer[String](WWWAuthenticateChallenge.bearer(AccountSettings.Realm)))
      .errorOut(ApiErrors.oneOfApiErrors)
      .out(setCookieOpt(clientCookieName))
      .out(header[Option[String]](sendToClientHeaderName))
      .serverSecurityLogicWithOutput[Unit, Future](token =>
        run(token, OAuthClient(token)) map {
          case r: OAuthClientSucceededResult =>
            val encoded = encodeClient(r.client)
            Right(
              st match {
                case HeaderST =>
                  (None, Some(encoded))
                case CookieST =>
                  (
                    Some(manager.clientSessionManager.createCookieWithValue(encoded).valueWithMeta),
                    None
                  )
              },
              ()
            )

          case _ => Left(resultToApiError(BearerAuthenticationFailed))
        }
      )
      .serverLogic { _ => _ =>
        Future.successful(
          Right(())
        )
      }

}

sealed trait ClientTokenRequest {
  def asMap(): Map[String, String]
}

case class AuthorizationCodeRequest(
  grant_type: String,
  code: String,
  redirect_uri: Option[String],
  client_id: String
) extends ClientTokenRequest {
  override def asMap(): Map[String, String] =
    Map(
      "grant_type"   -> grant_type,
      "code"         -> code,
      "redirect_uri" -> redirect_uri.getOrElse(""),
      "client_id"    -> client_id
    )
}

case class ClientCredentials(credentials: String, scope: Option[String] = None)
    extends ClientTokenRequest {
  override def asMap(): Map[String, String] =
    Map(
      "grant_type"  -> "client_credentials",
      "credentials" -> credentials,
      "scope"       -> scope.getOrElse("")
    )
}

case class RefreshToken(refreshToken: String) extends ClientTokenRequest {
  override def asMap(): Map[String, String] =
    Map(
      "grant_type"    -> "refresh_token",
      "refresh_token" -> refreshToken
    )
}

case class UnsupportedGrantType(grantType: String) extends ClientTokenRequest {
  override def asMap(): Map[String, String] =
    Map(
      "grant_type" -> grantType
    )
}

object ClientTokenRequest {
  def decode(form: Map[String, String]): ClientTokenRequest = {
    form.getOrElse("grant_type", "") match {
      case "authorization_code" =>
        AuthorizationCodeRequest(
          "authorization_code",
          form.getOrElse("code", ""),
          form.get("redirect_uri"),
          form.getOrElse("client_id", "")
        )
      case "client_credentials" =>
        ClientCredentials(
          form("credentials"),
          form.get("scope")
        )
      case "refresh_token" =>
        RefreshToken(form("refresh_token"))
      case other => UnsupportedGrantType(other)
    }
  }

  def encode(tokenRequest: ClientTokenRequest): Map[String, String] = tokenRequest.asMap()
}
