package app.softnetwork.payment.handlers

import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.account.handlers.{AccountDao, AccountHandler}
import app.softnetwork.account.message.AccountCommand
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.message.AccountMessages
import app.softnetwork.payment.message.AccountMessages.ProviderLoaded
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
    lookup(clientId) flatMap {
      case Some(uuid) =>
        ?(uuid, AccountMessages.LoadProvider(clientId)) map {
          case result: ProviderLoaded => Some(result.provider)
          case _                      => None
        }
      case _ => Future.successful(None)
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
