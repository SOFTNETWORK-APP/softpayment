package app.softnetwork.payment.persistence.typed

import app.softnetwork.account.handlers.MockGenerator

object MockSoftPaymentAccountBehavior extends SoftPaymentAccountBehavior with MockGenerator {
  override def persistenceId = "MockSoftPaymentAccount"
}
