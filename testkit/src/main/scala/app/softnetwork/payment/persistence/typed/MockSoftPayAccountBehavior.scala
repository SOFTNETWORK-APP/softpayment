package app.softnetwork.payment.persistence.typed

import app.softnetwork.account.handlers.MockGenerator

object MockSoftPayAccountBehavior extends SoftPayAccountBehavior with MockGenerator {
  override def persistenceId = "MockSoftPayAccount"
}
