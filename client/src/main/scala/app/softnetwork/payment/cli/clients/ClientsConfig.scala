package app.softnetwork.payment.cli.clients

object ClientsSubCommand extends Enumeration {
  type ClientsSubCommand = Value
  val Empty, List, Get, Set, Add, Remove = Value
}

case class ClientsConfig(
  subCommand: ClientsSubCommand.Value = ClientsSubCommand.Empty,
  clientId: Option[String] = None,
  apiKey: Option[String] = None
)
