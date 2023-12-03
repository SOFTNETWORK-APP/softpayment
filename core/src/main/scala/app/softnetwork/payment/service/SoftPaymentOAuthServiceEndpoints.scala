package app.softnetwork.payment.service

import akka.http.scaladsl.model.headers.BasicHttpCredentials
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccessTokenRefreshed,
  AccountErrorMessage,
  BearerAuthenticationFailed,
  Tokens
}
import app.softnetwork.account.service.OAuthServiceEndpoints
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.config.PaymentSettings.ClientSessionConfig
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.payment.message.AccountMessages.{
  GenerateClientToken,
  OAuthClient,
  OAuthClientSucceededResult,
  RefreshClientToken
}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.httpCookieToTapirCookieWithMeta
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{CookieST, HeaderST, SessionConfig}
import org.json4s.Formats
import org.softnetwork.session.model.JwtClaims
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir.EndpointIO.Example
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait SoftPaymentOAuthServiceEndpoints
    extends OAuthServiceEndpoints[JwtClaims]
    with SoftPaymentAccountTypeKey
    with ClientSession { _: SessionMaterials[JwtClaims] =>

  import app.softnetwork.serialization.serialization

  final implicit def sessionConfig: SessionConfig = ClientSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      token,
      me
    ) ++ services.map(signin) ++ services.map(backup)

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
      .serverLogic {
        case tokenRequest: ClientCredentials =>
          import tokenRequest._
          val httpCredentials = BasicHttpCredentials(credentials)
          val clientId = httpCredentials.username
          val clientSecret = httpCredentials.password
          run(clientId, GenerateClientToken(clientId, clientSecret)) map {
            case r: AccessTokenGenerated =>
              Right(
                Tokens(
                  r.accessToken.token,
                  r.accessToken.tokenType.toLowerCase(),
                  AccountSettings.OAuthSettings.accessToken.expirationTime * 60,
                  r.accessToken.refreshToken
                )
              )
            case error: AccountErrorMessage =>
              Left(ApiErrors.BadRequest(error.message))
            case _ => Left(ApiErrors.BadRequest("Unknown"))
          }
        case tokenRequest: RefreshToken =>
          import tokenRequest._
          run(refreshToken, RefreshClientToken(refreshToken)) map {
            case r: AccessTokenRefreshed =>
              Right(
                Tokens(
                  r.accessToken.token,
                  r.accessToken.tokenType.toLowerCase(),
                  AccountSettings.OAuthSettings.accessToken.expirationTime * 60,
                  r.accessToken.refreshToken
                )
              )
            case error: AccountErrorMessage =>
              Left(ApiErrors.BadRequest(error.message))
            case _ => Left(ApiErrors.BadRequest("Unknown"))
          }
        case tokenRequest: UnsupportedGrantType =>
          Future.successful(
            Left(ApiErrors.BadRequest(s"Unknown grant_type ${tokenRequest.grantType}"))
          )
      }

  override val me: ServerEndpoint[Any with AkkaStreams, Future] =
    endpoint.get
      .in(AccountSettings.OAuthPath / "me")
      .description("OAuth2 me endpoint")
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
