package app.softnetwork.payment.model

import app.softnetwork.account.model.{BasicAccount, BasicAccountCompanion}
import app.softnetwork.payment.serialization._

trait SoftPayAccountCompanion extends BasicAccountCompanion {

  def apply(account: Option[BasicAccount]): Option[SoftPayAccount] = {
    account match {
      case Some(a) => Some(a)
      case _       => None
    }
  }

}
