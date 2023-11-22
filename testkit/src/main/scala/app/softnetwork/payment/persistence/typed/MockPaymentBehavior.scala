package app.softnetwork.payment.persistence.typed

import app.softnetwork.payment.config.MangoPay
import app.softnetwork.payment.handlers.{
  GenericPaymentDao,
  MockPaymentDao,
  MockSoftPaymentAccountDao,
  SoftPaymentAccountDao
}
import app.softnetwork.payment.model.SoftPaymentAccount.Client
import app.softnetwork.payment.model.SoftPaymentAccount.Client.Provider

object MockPaymentBehavior extends GenericPaymentBehavior {
  override def persistenceId = s"Mock${super.persistenceId}"

  override lazy val paymentDao: GenericPaymentDao = MockPaymentDao

  override lazy val softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao

  override def defaultProvider: Client.Provider =
    MangoPay.softPaymentProvider.withProviderType(Provider.ProviderType.MOCK)
}
