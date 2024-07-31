package app.softnetwork.payment.config

import com.typesafe.config.{Config, ConfigFactory}
import configs.Configs

/** Created by smanciot on 05/07/2018.
  */
trait PaymentSettings {

  lazy val config: Config = ConfigFactory.load()

  lazy val PaymentConfig: Payment.Config = {
    Configs[Payment.Config].get(config, "payment").toEither match {
      case Left(configError) =>
        Console.err.println(s"Something went wrong with the provided arguments $configError")
        throw configError.configException
      case Right(paymentConfig) => paymentConfig
    }
  }

}

object PaymentSettings extends PaymentSettings {
  def apply(conf: Config): PaymentSettings = new PaymentSettings {
    override lazy val config: Config = conf
  }

  def apply(): PaymentSettings = new PaymentSettings {}
}
