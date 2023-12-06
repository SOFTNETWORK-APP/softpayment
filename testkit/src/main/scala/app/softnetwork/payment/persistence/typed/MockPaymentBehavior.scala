package app.softnetwork.payment.persistence.typed

import app.softnetwork.payment.handlers.{
  MockPaymentDao,
  MockSoftPaymentAccountDao,
  PaymentDao,
  SoftPaymentAccountDao
}

object MockPaymentBehavior extends PaymentBehavior {
  override def persistenceId = s"Mock${super.persistenceId}"

  override lazy val paymentDao: PaymentDao = MockPaymentDao

  override lazy val softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao

}
