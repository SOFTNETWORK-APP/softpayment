package app.softnetwork.payment.config

import configs.Configs

object MangoPaySettings extends PaymentSettings {

  lazy val MangoPayConfig: MangoPay.Config =
    Configs[MangoPay.Config].get(config, "payment.mangopay").toEither match {
      case Left(configError) =>
        logger.error(s"Something went wrong with the provided arguments $configError")
        throw configError.configException
      case Right(mangoPayConfig) => mangoPayConfig
    }

}
