package app.softnetwork.payment.cli.signup

import app.softnetwork.payment.api.ProviderType
import app.softnetwork.payment.cli.{CliConfig, Command}
import scopt.OParser

case class SignUpClientConfig(
  principal: String = "",
  credentials: Array[Char] = Array.emptyCharArray,
  providerId: String = "",
  providerApiKey: Array[Char] = Array.emptyCharArray,
  providerType: Option[ProviderType] = None
)

object SignUpClientConfig extends CliConfig[SignUpClientConfig] {

  val command: String = "signup"

  val parser: OParser[Unit, SignUpClientConfig] = {
    val builder = OParser.builder[SignUpClientConfig]
    import builder._
    OParser.sequence(
      programName(s"payment $command"),
      head("payment", command, "[options]"),
      opt[String]('p', "principal")
        .action((x, c) => c.copy(principal = x))
        .text("principal")
        .required(),
      opt[String]('c', "credentials")
        .action((x, c) => c.copy(credentials = x.toCharArray))
        .text("credentials")
        .required()
        /*.validate(x =>
          import app.softnetwork.account.config.AccountSettings.passwordRules
          passwordRules().validate(x) match {
            case Right(_)    => success
            case Left(error) => failure(error.mkString(", "))
          }
        )*/,
      opt[String]('i', "providerId")
        .action((x, c) => c.copy(providerId = x))
        .text("provider Id")
        .required(),
      opt[String]('k', "providerApiKey")
        .action((x, c) => c.copy(providerApiKey = x.toCharArray))
        .text("provider Api Key")
        .required(),
      opt[String]('t', "providerType")
        .action((x, c) => c.copy(providerType = ProviderType.fromName(x)))
        .text("provider type")
        .optional()
    )
  }

  def parse(args: Seq[String]): Option[SignUpClientConfig] = {
    OParser.parse(parser, args, SignUpClientConfig())
  }

  def runner: Command[SignUpClientConfig] = SignUpClient
}
