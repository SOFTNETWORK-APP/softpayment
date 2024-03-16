package app.softnetwork.payment.handlers

import org.slf4j.{Logger, LoggerFactory}

trait MockPaymentDao extends PaymentDao with MockPaymentTypeKey

object MockPaymentDao extends MockPaymentDao {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
