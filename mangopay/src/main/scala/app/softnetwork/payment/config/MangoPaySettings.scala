package app.softnetwork.payment.config

import com.typesafe.config.{Config, ConfigFactory}
import configs.Configs

trait MangoPaySettings {

  lazy val config: Config = ConfigFactory.load()

  lazy val MangoPayConfig: MangoPay.Config =
    Configs[MangoPay.Config].get(config, "payment.mangopay").toEither match {
      case Left(configError) =>
        Console.err.println(s"Something went wrong with the provided arguments $configError")
        throw configError.configException
      case Right(mangoPayConfig) =>
        mangoPayConfig.withPaymentConfig(PaymentSettings(config).PaymentConfig)
    }

}

object MangoPaySettings extends MangoPaySettings {
  def apply(conf: Config): MangoPaySettings = new MangoPaySettings {
    override lazy val config: Config = conf
  }
}
