package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.account.handlers.{AccountDao, AccountHandler}
import app.softnetwork.account.message.{
  AccessTokenGenerated,
  AccessTokenRefreshed,
  AccountActivated,
  AccountCommand,
  AccountCreated,
  AccountErrorMessage,
  Activate,
  Tokens
}
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.message.AccountMessages
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.persistence.typed.SoftPayAccountBehavior
import app.softnetwork.persistence.generateUUID
import app.softnetwork.persistence.typed.CommandTypeKey
import app.softnetwork.session.model.JwtClaimsEncoder
import com.softwaremill.session.SessionConfig
import org.softnetwork.session.model.{ApiKey, JwtClaims}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait SoftPayAccountDao extends AccountDao with SoftPayAccountHandler {
  @InternalApi
  private[payment] def loadProvider(
    clientId: String
  )(implicit
    system: ActorSystem[_]
  ): Future[Option[SoftPayAccount.SoftPayClient.SoftPayProvider]] = {
    implicit val ec: ExecutionContext = system.executionContext
    loadClient(clientId).map(_.map(_.provider))
  }

  @InternalApi
  private[payment] def loadClient(
    clientId: String
  )(implicit system: ActorSystem[_]): Future[Option[SoftPayAccount.SoftPayClient]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(clientId, AccountMessages.LoadClient(clientId)) map {
      case result: AccountMessages.ClientLoaded => Some(result.client)
      case _                                    => None
    }
  }

  @InternalApi
  private[payment] def oauthClient(
    token: String
  )(implicit system: ActorSystem[_]): Future[Option[SoftPayAccount.SoftPayClient]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(token, AccountMessages.OAuthClient(token)) map {
      case result: AccountMessages.OAuthClientSucceededResult => Some(result.client)
      case _                                                  => None
    }
  }

  @InternalApi
  private[payment] def generateClientTokens(
    clientId: String,
    clientSecret: String,
    scope: Option[String] = None
  )(implicit system: ActorSystem[_]): Future[Either[String, Tokens]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(clientId, AccountMessages.GenerateClientToken(clientId, clientSecret, scope)) map {
      case result: AccessTokenGenerated =>
        import result._
        Right(
          Tokens(
            accessToken.token,
            accessToken.tokenType.toLowerCase(),
            accessToken.expiresIn,
            accessToken.refreshToken,
            accessToken.refreshExpiresIn
          )
        )
      case error: AccountErrorMessage => Left(error.message)
      case _                          => Left("unknown")
    }
  }

  @InternalApi
  private[payment] def refreshClientTokens(
    refreshToken: String
  )(implicit system: ActorSystem[_]): Future[Either[String, Tokens]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(refreshToken, AccountMessages.RefreshClientToken(refreshToken)) map {
      case result: AccessTokenRefreshed =>
        import result._
        Right(
          Tokens(
            accessToken.token,
            accessToken.tokenType.toLowerCase(),
            accessToken.expiresIn,
            accessToken.refreshToken,
            accessToken.refreshExpiresIn
          )
        )
      case error: AccountErrorMessage => Left(error.message)
      case _                          => Left("unknown")
    }
  }

  @InternalApi
  private[payment] def signUpClient(
    signUp: AccountMessages.SoftPaySignUp
  )(implicit system: ActorSystem[_]): Future[Either[String, SoftPayAccount.SoftPayClient]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(
      generateUUID(Some(signUp.login)),
      signUp
    ) map {
      case result: AccountCreated =>
        Right(
          result.account
            .asInstanceOf[SoftPayAccount]
            .clients
            .find(_.provider.providerId == signUp.provider.providerId)
            .get
        )
      case error: AccountErrorMessage => Left(error.message)
      case _                          => Left("unknown")
    }
  }

  @InternalApi
  private[payment] def activateClient(
    activation: Activate
  )(implicit system: ActorSystem[_]): Future[Either[String, Boolean]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(activation.token, activation) map {
      case result: AccountActivated   => Right(result.account.status.isActive)
      case error: AccountErrorMessage => Left(error.message)
      case _                          => Left("unknown")
    }
  }

  @InternalApi
  private[payment] def loadApiKey(
    clientId: String
  )(implicit system: ActorSystem[_]): Future[Option[ApiKey]] = {
    implicit val ec: ExecutionContext = system.executionContext
    ??(clientId, AccountMessages.LoadApiKey(clientId)) map {
      case result: AccountMessages.ApiKeyLoaded => Some(result.apiKey)
      case _                                    => None
    }
  }

  @InternalApi
  private[payment] def registerAccountWithProvider(
    provider: SoftPayAccount.SoftPayClient.SoftPayProvider
  )(implicit
    system: ActorSystem[_]
  ): Future[Option[SoftPayAccount]] = {
    implicit val ec: ExecutionContext = system.executionContext
    !?(AccountMessages.RegisterAccountWithProvider(provider)) map {
      case result: AccountMessages.AccountWithProviderRegistered => Some(result.account)
      case _                                                     => None
    }
  }

  @InternalApi
  private[payment] def authenticateClient(
    token: Option[String]
  )(implicit system: ActorSystem[_]): Future[Option[SoftPayAccount.SoftPayClient]] = {
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
                          case None => Future.successful(None)
                          case some => Future.successful(some)
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

trait SoftPayAccountTypeKey extends CommandTypeKey[AccountCommand] {
  override def TypeKey(implicit tTag: ClassTag[AccountCommand]): EntityTypeKey[AccountCommand] =
    SoftPayAccountBehavior.TypeKey
}

trait SoftPayAccountHandler extends AccountHandler with SoftPayAccountTypeKey

object SoftPayAccountDao extends SoftPayAccountDao {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
