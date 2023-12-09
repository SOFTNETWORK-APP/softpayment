package app.softnetwork.payment.api.config

import akka.actor.typed.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.softnetwork.session.model.JwtClaims

import java.nio.file.{Path, Paths}

case class PaymentClientSettings(clientId: String, apiKey: String) {
  def generateToken(): String =
    JwtClaims.newSession
      .withClientId(clientId)
      .encode(clientId, apiKey)

}

object PaymentClientSettings {
  def apply(system: ActorSystem[_]): PaymentClientSettings = {
    val softPaymentHome =
      sys.env.getOrElse("SOFT_PAYMENT_HOME", System.getProperty("user.home") + "/soft-payment")
    val clientConfigFile: Path = Paths.get(s"$softPaymentHome/config/application.conf")
    val systemConfig = system.settings.config.getConfig("payment")
    val clientConfig: Config = {
      if (clientConfigFile.toFile.exists() && !systemConfig.hasPath("test")) {
        ConfigFactory
          .parseFile(clientConfigFile.toFile)
          .withFallback(systemConfig)
      } else {
        systemConfig
      }
    }
    PaymentClientSettings(
      clientId = clientConfig.getString("client-id"),
      apiKey = clientConfig.getString("api-key")
    )
  }
}
