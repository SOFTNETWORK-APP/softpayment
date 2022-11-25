package app.softnetwork.payment.model

import app.softnetwork.persistence._

/** Created by smanciot on 18/08/2018.
  */
trait TransactionCompanion {
  def apply(): Transaction = {
    Transaction.defaultInstance.copy(
      createdDate = now(),
      lastUpdated = now()
    )
  }
}
