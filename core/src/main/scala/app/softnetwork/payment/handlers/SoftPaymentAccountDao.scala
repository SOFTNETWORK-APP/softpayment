package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.account.config.AccountSettings
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
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait SoftPaymentAccountDao extends AccountDao { _: AccountHandler =>
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
        Some(
          Tokens(
            result.accessToken.token,
            result.accessToken.tokenType.toLowerCase(),
            AccountSettings.OAuthSettings.accessToken.expirationTime * 60,
            result.accessToken.refreshToken
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
        Some(
          Tokens(
            result.accessToken.token,
            result.accessToken.tokenType.toLowerCase(),
            AccountSettings.OAuthSettings.accessToken.expirationTime * 60,
            result.accessToken.refreshToken
          )
        )
      case _ => None
    }
  }
}

trait SoftPaymentAccountTypeKey extends CommandTypeKey[AccountCommand] {
  override def TypeKey(implicit tTag: ClassTag[AccountCommand]): EntityTypeKey[AccountCommand] =
    SoftPaymentAccountBehavior.TypeKey
}

object SoftPaymentAccountDao
    extends SoftPaymentAccountDao
    with AccountHandler
    with SoftPaymentAccountTypeKey {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
