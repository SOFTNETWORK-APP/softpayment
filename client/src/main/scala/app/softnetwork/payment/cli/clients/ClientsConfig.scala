package app.softnetwork.payment.cli.clients

import app.softnetwork.payment.cli.{CliConfig, Command}
import scopt.OParser

object ClientsSubCommand extends Enumeration {
  type ClientsSubCommand = Value
  val Empty, List, Get, Set, Add, Remove = Value
}

case class ClientsConfig(
  subCommand: ClientsSubCommand.Value = ClientsSubCommand.Empty,
  clientId: Option[String] = None,
  apiKey: Option[String] = None
)

object ClientsConfig extends CliConfig[ClientsConfig] {

  val command: String = "clients"

  val parser: OParser[Unit, ClientsConfig] = {
    val builder = OParser.builder[ClientsConfig]
    import builder._
    OParser.sequence(
      programName(s"$shell $command"),
      head(shell, command, "[options]"),
      cmd("list")
        .action((_, c) => c.copy(subCommand = ClientsSubCommand.List))
        .text("list all clients"),
      cmd("get")
        .action((_, c) => c.copy(subCommand = ClientsSubCommand.Get))
        .text("get client by id")
        .children(
          opt[String]('i', "clientId")
            .action((x, c) => c.copy(clientId = Some(x)))
            .text("client Id")
            .required()
        ),
      cmd("remove")
        .action((_, c) => c.copy(subCommand = ClientsSubCommand.Remove))
        .text("remove client by id")
        .children(
          opt[String]('i', "clientId")
            .action((x, c) => c.copy(clientId = Some(x)))
            .text("client Id")
            .required()
        ),
      cmd("add")
        .action((_, c) => c.copy(subCommand = ClientsSubCommand.Add))
        .text("add api key")
        .children(
          opt[String]('i', "clientId")
            .action((x, c) => c.copy(clientId = Some(x)))
            .text("client Id")
            .required(),
          opt[String]('s', "apiKey")
            .action((x, c) => c.copy(apiKey = Some(x)))
            .text("api Key")
            .required()
        ),
      cmd("set")
        .action((_, c) => c.copy(subCommand = ClientsSubCommand.Set))
        .text("set client by id")
        .children(
          opt[String]('i', "clientId")
            .action((x, c) => c.copy(clientId = Some(x)))
            .text("client Id")
            .required()
        )
    )
  }

  def parse(args: Seq[String]): Option[ClientsConfig] = {
    OParser.parse(parser, args, ClientsConfig())
  }

  def runner: Command[ClientsConfig] = Clients
}
