package app.softnetwork.payment.handlers

import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.account.handlers.AccountHandler
import app.softnetwork.account.message.AccountCommand
import app.softnetwork.payment.persistence.typed.MockSoftPaymentAccountBehavior
import app.softnetwork.persistence.typed.CommandTypeKey
import org.slf4j.{Logger, LoggerFactory}

import scala.reflect.ClassTag

trait MockBasicAccountTypeKey extends CommandTypeKey[AccountCommand] {
  override def TypeKey(implicit tTag: ClassTag[AccountCommand]): EntityTypeKey[AccountCommand] =
    MockSoftPaymentAccountBehavior.TypeKey
}

object MockSoftPaymentAccountDao
    extends SoftPaymentAccountDao
    with AccountHandler
    with MockBasicAccountTypeKey {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
