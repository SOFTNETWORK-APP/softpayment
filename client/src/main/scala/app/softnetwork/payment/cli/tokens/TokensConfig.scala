package app.softnetwork.payment.cli.tokens

import app.softnetwork.payment.cli.{CliConfig, Command}
import scopt.OParser

case class TokensConfig(
  refreshToken: Option[String] = None
)

object TokensConfig extends CliConfig[TokensConfig] {

  val command: String = "tokens"

  val parser: OParser[Unit, TokensConfig] = {
    val builder = OParser.builder[TokensConfig]
    import builder._
    OParser.sequence(
      programName(s"payment $command"),
      head("payment", command, "[options]"),
      opt[String]('r', "refreshToken")
        .action((x, c) => c.copy(refreshToken = Some(x)))
        .text("refresh token")
    )
  }

  def parse(args: Seq[String]): Option[TokensConfig] = {
    OParser.parse(parser, args, TokensConfig())
  }

  def runner: Command[TokensConfig] = Tokens
}
