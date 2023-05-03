package app.softnetwork.payment.handlers

import org.slf4j.{Logger, LoggerFactory}

object MangoPayPaymentHandler extends MangoPayPaymentHandler {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
}

trait MangoPayPaymentHandler extends GenericPaymentHandler with MangoPayPaymentTypeKey
