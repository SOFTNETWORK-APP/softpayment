package app.softnetwork.payment.handlers

import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.account.message.AccountCommand
import app.softnetwork.payment.persistence.typed.MockSoftPayAccountBehavior
import app.softnetwork.persistence.typed.CommandTypeKey
import org.slf4j.{Logger, LoggerFactory}

import scala.reflect.ClassTag

trait MockSoftPayAccountTypeKey extends CommandTypeKey[AccountCommand] {
  override def TypeKey(implicit tTag: ClassTag[AccountCommand]): EntityTypeKey[AccountCommand] =
    MockSoftPayAccountBehavior.TypeKey
}

trait MockSoftPayAccountHandler extends SoftPayAccountHandler with MockSoftPayAccountTypeKey

trait MockSoftPayAccountDao extends SoftPayAccountDao with MockSoftPayAccountTypeKey

object MockSoftPayAccountDao extends MockSoftPayAccountDao {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
