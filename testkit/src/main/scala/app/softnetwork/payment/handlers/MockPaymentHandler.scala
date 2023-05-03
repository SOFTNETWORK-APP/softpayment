package app.softnetwork.payment.handlers

import org.slf4j.{Logger, LoggerFactory}

object MockPaymentHandler extends MockPaymentHandler {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}

trait MockPaymentHandler extends GenericPaymentHandler with MockPaymentTypeKey
