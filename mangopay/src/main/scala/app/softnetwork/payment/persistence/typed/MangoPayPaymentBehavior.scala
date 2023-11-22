package app.softnetwork.payment.persistence.typed

import app.softnetwork.payment.config.MangoPay
import app.softnetwork.payment.handlers.{GenericPaymentDao, MangoPayPaymentDao}
import app.softnetwork.payment.model.SoftPaymentAccount.Client

case object MangoPayPaymentBehavior extends MangoPayPaymentBehavior {
  override lazy val paymentDao: GenericPaymentDao = MangoPayPaymentDao
}

trait MangoPayPaymentBehavior extends GenericPaymentBehavior {
  override def defaultProvider: Client.Provider = MangoPay.softPaymentProvider
}
