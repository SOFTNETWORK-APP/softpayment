package app.softnetwork.payment.handlers

import app.softnetwork.kv.handlers.{KvDao, KvHandler}
import app.softnetwork.payment.persistence.typed.PaymentKvBehavior
import org.slf4j.{Logger, LoggerFactory}

trait PaymentKvDao extends KvDao with KvHandler with PaymentKvBehavior

object PaymentKvDao extends PaymentKvDao {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
