package app.softnetwork.payment.config

import com.typesafe.config.{Config, ConfigFactory}
import configs.Configs

trait StripeSettings {

  lazy val config: Config = ConfigFactory.load()

  lazy val StripeApiConfig: StripeApi.Config =
    Configs[StripeApi.Config].get(config, "payment.stripe").toEither match {
      case Left(configError) =>
        Console.err.println(s"Something went wrong with the provided arguments $configError")
        throw configError.configException
      case Right(stripeConfig) =>
        stripeConfig.withPaymentConfig(PaymentSettings(config).PaymentConfig)
    }

}

object StripeSettings extends StripeSettings {
  def apply(conf: Config): StripeSettings = new StripeSettings {
    override lazy val config: Config = conf
  }

  def apply(): StripeSettings = new StripeSettings {}
}
