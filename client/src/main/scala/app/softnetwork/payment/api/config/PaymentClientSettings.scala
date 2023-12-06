package app.softnetwork.payment.api.config

import akka.actor.typed.ActorSystem
import org.softnetwork.session.model.JwtClaims

case class PaymentClientSettings(clientId: String, apiKey: String) {
  def generateToken(): String =
    JwtClaims.newSession
      .withClientId(clientId)
      .encode(clientId, apiKey)

}

object PaymentClientSettings {
  def apply(system: ActorSystem[_]): PaymentClientSettings = {
    val clientConfig = system.settings.config.getConfig("payment")
    PaymentClientSettings(
      clientId = clientConfig.getString("client-id"),
      apiKey = clientConfig.getString("api-key")
    )
  }
}
