package app.softnetwork.payment.api.config

import akka.actor.typed.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.softnetwork.session.model.JwtClaims

import java.nio.file.{Path, Paths}

case class SoftPayClientSettings(clientId: String, apiKey: String) {
  def generateToken(): String =
    JwtClaims.newSession
      .withClientId(clientId)
      .encode(clientId, apiKey)

  def write(): Unit = {
    ApiKeys.+(clientId, apiKey)
    select()
  }

  private[payment] def select(): Unit = {
    val config = SoftPayClientSettings.SP_ROOT + "/config"
    Paths.get(config).toFile.mkdirs()
    val application = Paths.get(config + "/application.conf").toFile
    application.createNewFile()
    val applicationWriter = new java.io.BufferedWriter(new java.io.FileWriter(application))
    applicationWriter.write(
      s"""client-id = "$clientId"
         |api-key = "$apiKey"
         |""".stripMargin
    )
    applicationWriter.close()
  }

}

object SoftPayClientSettings {
  lazy val SP_ROOT: String =
    sys.env.getOrElse(
      "SP_ROOT",
      Option(System.getProperty("user.home") + "/soft-pay").getOrElse(".")
    )

  lazy val SP_CONFIG: String = sys.env.getOrElse("SP_CONFIG", SP_ROOT + "/config")

  lazy val SP_SECRETS: String = sys.env.getOrElse("SP_SECRETS", SP_ROOT + "/secrets")

  def apply(system: ActorSystem[_]): SoftPayClientSettings = {
    val clientConfigFile: Path = Paths.get(s"$SP_CONFIG/application.conf")
    val systemConfig = system.settings.config.getConfig("payment")
    val clientConfig: Config = {
      if (
        clientConfigFile.toFile
          .exists() && (!systemConfig.hasPath("test") || !systemConfig.getBoolean("test"))
      ) {
        ConfigFactory
          .parseFile(clientConfigFile.toFile)
          .withFallback(systemConfig)
      } else {
        systemConfig
      }
    }
    SoftPayClientSettings(
      clientId = clientConfig.getString("client-id"),
      apiKey = clientConfig.getString("api-key")
    )
  }

  def select(clientId: String): Option[SoftPayClientSettings] = {
    ApiKeys.list().get(clientId) match {
      case Some(apiKey) =>
        val softPayClientSettings = SoftPayClientSettings(clientId, apiKey)
        softPayClientSettings.select()
        Some(softPayClientSettings)
      case None => None
    }
  }
}
