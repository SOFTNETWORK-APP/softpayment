package app.softnetwork.payment.persistence.typed

import app.softnetwork.payment.handlers.{GenericPaymentDao, MockPaymentDao}
import app.softnetwork.payment.spi.MockMangoPayProvider

case object MockPaymentBehavior extends GenericPaymentBehavior with MockMangoPayProvider {
  override def persistenceId = s"Mock${super.persistenceId}"

  override lazy val paymentDao: GenericPaymentDao = MockPaymentDao
}
