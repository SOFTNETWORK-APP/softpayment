package app.softnetwork.payment.model

import app.softnetwork.account.model.{BasicAccountProfile, Profile}
import org.softnetwork.session.model.ApiKey

trait SoftPayAccountDecorator { _: SoftPayAccount =>

  override def newProfile(name: String): Profile =
    BasicAccountProfile.defaultInstance.withName(name)

  lazy val apiKeys: Seq[ApiKey] =
    this.clients.map(client => ApiKey(client.clientId, client.clientApiKey))
}
