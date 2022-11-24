package app.softnetwork.payment.persistence.typed

import app.softnetwork.kv.persistence.typed.KeyValueBehavior
import app.softnetwork.payment.config.PaymentSettings.AkkaNodeRole

trait PaymentKvBehavior extends KeyValueBehavior {
  override def persistenceId: String = "PaymentKeys"

  /**
    *
    * @return node role required to start this actor
    */
  override lazy val role: String = AkkaNodeRole

}

object PaymentKvBehavior extends PaymentKvBehavior
