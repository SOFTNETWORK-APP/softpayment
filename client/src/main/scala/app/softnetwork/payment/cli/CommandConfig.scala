package app.softnetwork.payment.cli

import scopt.OParser

trait CommandConfig {
  def command: String
}

trait CliConfig[T] extends CommandConfig {
  def parser: OParser[Unit, T]

  def usage(): String = OParser.usage(parser)

  def parse(args: Seq[String]): Option[T]
}
