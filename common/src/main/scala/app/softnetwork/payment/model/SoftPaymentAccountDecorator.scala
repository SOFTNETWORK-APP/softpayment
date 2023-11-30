package app.softnetwork.payment.model

import app.softnetwork.account.model.{BasicAccountProfile, Profile}
import app.softnetwork.session.model.ApiKey

trait SoftPaymentAccountDecorator { _: SoftPaymentAccount =>

  override def newProfile(name: String): Profile =
    BasicAccountProfile.defaultInstance.withName(name)

  lazy val apiKeys: Seq[ApiKey] =
    this.clients.map(client => ApiKey(client.clientId, client.clientApiKey))
}
