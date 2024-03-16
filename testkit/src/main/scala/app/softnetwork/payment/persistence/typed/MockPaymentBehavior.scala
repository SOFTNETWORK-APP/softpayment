package app.softnetwork.payment.persistence.typed

import app.softnetwork.payment.handlers.{
  MockPaymentDao,
  MockSoftPayAccountDao,
  PaymentDao,
  SoftPayAccountDao
}

object MockPaymentBehavior extends PaymentBehavior {
  override def persistenceId = s"Mock${super.persistenceId}"

  override lazy val paymentDao: PaymentDao = MockPaymentDao

  override lazy val softPayAccountDao: SoftPayAccountDao = MockSoftPayAccountDao

}
