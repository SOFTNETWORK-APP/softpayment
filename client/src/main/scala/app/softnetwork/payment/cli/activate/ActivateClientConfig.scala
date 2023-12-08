package app.softnetwork.payment.cli.activate

import app.softnetwork.payment.cli.{CliConfig, Command}
import scopt.OParser

case class ActivateClientConfig(token: String = "")

object ActivateClientConfig extends CliConfig[ActivateClientConfig] {

  val command: String = "activate"

  val parser: OParser[Unit, ActivateClientConfig] = {
    val builder = OParser.builder[ActivateClientConfig]
    import builder._
    OParser.sequence(
      programName(s"payment $command"),
      head("payment", command, "[options]"),
      opt[String]('t', "token")
        .action((x, c) => c.copy(token = x))
        .text("token")
        .required()
    )
  }

  def parse(args: Seq[String]): Option[ActivateClientConfig] = {
    OParser.parse(parser, args, ActivateClientConfig())
  }

  def runner: Command[ActivateClientConfig] = ActivateClient
}
