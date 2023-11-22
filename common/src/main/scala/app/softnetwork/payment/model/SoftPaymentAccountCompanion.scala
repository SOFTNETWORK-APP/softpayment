package app.softnetwork.payment.model

import app.softnetwork.account.model.{BasicAccount, BasicAccountCompanion}
import app.softnetwork.payment.serialization._

trait SoftPaymentAccountCompanion extends BasicAccountCompanion {

  def apply(account: Option[BasicAccount]): Option[SoftPaymentAccount] = {
    account match {
      case Some(a) => Some(a)
      case _       => None
    }
  }

}
