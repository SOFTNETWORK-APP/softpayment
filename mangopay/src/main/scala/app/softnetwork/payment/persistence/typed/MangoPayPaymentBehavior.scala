package app.softnetwork.payment.persistence.typed

import app.softnetwork.payment.handlers.{GenericPaymentDao, MangoPayPaymentDao}
import app.softnetwork.payment.spi.MangoPayProvider

case object MangoPayPaymentBehavior extends MangoPayPaymentBehavior {
  override lazy val paymentDao: GenericPaymentDao = MangoPayPaymentDao
}

trait MangoPayPaymentBehavior extends GenericPaymentBehavior with MangoPayProvider