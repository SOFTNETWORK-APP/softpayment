package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.account.handlers.{AccountDao, AccountHandler}
import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccessTokenRefreshed,
  AccountCommand,
  Tokens
}
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.message.AccountMessages
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.payment.persistence.typed.SoftPaymentAccountBehavior
import app.softnetwork.persistence.typed.CommandTypeKey
import app.softnetwork.session.model.JwtClaimsEncoder
import com.softwaremill.session.SessionConfig
import org.softnetwork.session.model.{ApiKey, JwtClaims}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait SoftPaymentAccountDao extends AccountDao with SoftPaymentAccountHandler {
  @InternalApi
  private[payment] def loadProvider(
    clientId: String
  )(implicit system: ActorSystem[_]): Future[Option[SoftPaymentAccount.Client.Provider]] = {
    implicit val ec: ExecutionContext = system.executionContext
    loadClient(clientId).map(_.map(_.provider))
  }

  @InternalApi
  private[payment] def loadClient(
    clientId: String
  )(implicit system: ActorSystem[_]): Future[Option[SoftPaymentAccount.Client]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(clientId, AccountMessages.LoadClient(clientId)) map {
      case result: AccountMessages.ClientLoaded => Some(result.client)
      case _                                    => None
    }
  }

  @InternalApi
  private[payment] def oauthClient(
    token: String
  )(implicit system: ActorSystem[_]): Future[Option[SoftPaymentAccount.Client]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(token, AccountMessages.OAuthClient(token)) map {
      case result: AccountMessages.OAuthClientSucceededResult => Some(result.client)
      case _                                                  => None
    }
  }

  def generateClientToken(
    clientId: String,
    clientSecret: String,
    scope: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Option[Tokens]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(clientId, AccountMessages.GenerateClientToken(clientId, clientSecret, scope)) map {
      case result: AccessTokenGenerated =>
        import result._
        Some(
          Tokens(
            accessToken.token,
            accessToken.tokenType.toLowerCase(),
            accessToken.expiresIn,
            accessToken.refreshToken,
            accessToken.refreshExpiresIn
          )
        )
      case _ => None
    }
  }

  def refreshClientToken(
    refreshToken: String
  )(implicit system: ActorSystem[_]): Future[Option[Tokens]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(refreshToken, AccountMessages.RefreshClientToken(refreshToken)) map {
      case result: AccessTokenRefreshed =>
        import result._
        Some(
          Tokens(
            accessToken.token,
            accessToken.tokenType.toLowerCase(),
            accessToken.expiresIn,
            accessToken.refreshToken,
            accessToken.refreshExpiresIn
          )
        )
      case _ => None
    }
  }

  def loadApiKey(
    clientId: String
  )(implicit system: ActorSystem[_]): Future[Option[ApiKey]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(clientId, AccountMessages.LoadApiKey(clientId)) map {
      case result: AccountMessages.ApiKeyLoaded => Some(result.apiKey)
      case _                                    => None
    }
  }

  def registerProviderAccount(provider: SoftPaymentAccount.Client.Provider)(implicit
    system: ActorSystem[_]
  ): Future[Option[SoftPaymentAccount]] = {
    implicit val ec: ExecutionContext = system.executionContext
    !?(AccountMessages.RegisterProviderAccount(provider)) map {
      case result: AccountMessages.ProviderAccountRegistered => Some(result.account)
      case _                                                 => None
    }
  }

  def authenticateClient(
    token: Option[String]
  )(implicit system: ActorSystem[_]): Future[Option[SoftPaymentAccount.Client]] = {
    token match {
      case Some(value) =>
        implicit val ec: ExecutionContext = system.executionContext
        oauthClient(value) flatMap {
          case Some(client) => Future.successful(Some(client))
          case _ =>
            val t = JwtClaims(value)
            t.clientId match {
              case Some(clientId) =>
                loadApiKey(clientId) flatMap {
                  case Some(apiKey) if apiKey.clientSecret.isDefined =>
                    val config = SessionConfig.default(apiKey.getClientSecret)
                    JwtClaimsEncoder
                      .decode(
                        value,
                        config.copy(jwt =
                          config.jwt.copy(
                            issuer = t.issuer.orElse(config.jwt.issuer),
                            subject = t.subject.orElse(config.jwt.subject),
                            audience = t.aud.orElse(config.jwt.audience)
                          )
                        )
                      )
                      .toOption match {
                      case Some(result) if result.signatureMatches =>
                        loadClient(clientId) flatMap {
                          case Some(client) => Future.successful(Some(client))
                          case _            => Future.successful(None)
                        }
                      case _ => Future.successful(None)
                    }
                  case _ => Future.successful(None)
                }
              case _ => Future.successful(None)
            }
        }
      case _ => Future.successful(None)
    }
  }
}

trait SoftPaymentAccountTypeKey extends CommandTypeKey[AccountCommand] {
  override def TypeKey(implicit tTag: ClassTag[AccountCommand]): EntityTypeKey[AccountCommand] =
    SoftPaymentAccountBehavior.TypeKey
}

trait SoftPaymentAccountHandler extends AccountHandler with SoftPaymentAccountTypeKey

object SoftPaymentAccountDao extends SoftPaymentAccountDao {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
