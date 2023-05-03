package app.softnetwork.payment.handlers

import org.slf4j.{Logger, LoggerFactory}

object MangoPayPaymentDao extends GenericPaymentDao with MangoPayPaymentHandler {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}
