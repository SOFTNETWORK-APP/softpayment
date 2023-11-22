package app.softnetwork.payment.model

import app.softnetwork.account.model.{BasicAccountProfile, Profile}

trait SoftPaymentAccountDecorator { _: SoftPaymentAccount =>

  override def newProfile(name: String): Profile =
    BasicAccountProfile.defaultInstance.withName(name)

}
