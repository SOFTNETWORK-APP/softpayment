package app.softnetwork.payment.cli.clients

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.config.{ApiKeys, SoftPayClientSettings}
import app.softnetwork.payment.cli.Cmd
import scopt.OParser

import scala.concurrent.Future

object ClientsCmd extends Cmd[ClientsConfig] {

  val name: String = "clients"

  val parser: OParser[Unit, ClientsConfig] = {
    val builder = OParser.builder[ClientsConfig]
    import builder._
    OParser.sequence(
      programName(s"$shell $name"),
      head(shell, name, "[options]"),
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

  override def run(
    config: ClientsConfig
  )(implicit system: ActorSystem[_]): Future[(Int, Option[String])] = {
    config.subCommand match {
      case ClientsSubCommand.List =>
        Future.successful(
          (
            0,
            Some(s"""
             |Available clients:
             |\t${ApiKeys.list().filter(_._2.trim.nonEmpty).keys.mkString("\n\t")}
             |""".stripMargin)
          )
        )
      case ClientsSubCommand.Set =>
        config.clientId match {
          case Some(clientId) =>
            Future.successful(SoftPayClientSettings.select(clientId) match {
              case Some(softPayClientSettings) =>
                (
                  0,
                  Some(s"""
                   |Client $clientId selected successfully!
                   |\tapiKey: ${softPayClientSettings.apiKey}
                   |""".stripMargin)
                )
              case None =>
                (
                  1,
                  Some(s"""
                   |apiKey not found for $clientId!
                   |""".stripMargin)
                )
            })
        }
      case ClientsSubCommand.Get =>
        config.clientId match {
          case Some(clientId) =>
            Future.successful(ApiKeys.get(clientId) match {
              case Some(apiKey) =>
                (
                  0,
                  Some(s"""
                   |Client $clientId loaded successfully!
                   |\tapiKey: $apiKey
                   |""".stripMargin)
                )
              case None =>
                (
                  1,
                  Some(s"""
                   |apiKey not found for $clientId!
                   |""".stripMargin)
                )
            })
        }
      case ClientsSubCommand.Add =>
        config.clientId match {
          case Some(clientId) =>
            ApiKeys.+(clientId, config.apiKey.getOrElse("")) match {
              case _ =>
                Future.successful(
                  (
                    0,
                    Some(s"""
                     |Client $clientId added successfully!
                     |""".stripMargin)
                  )
                )
            }
        }
      case ClientsSubCommand.Remove =>
        config.clientId match {
          case Some(clientId) =>
            ApiKeys.-(clientId) match {
              case _ =>
                Future.successful(
                  (
                    0,
                    Some(s"""
                     |Client $clientId removed successfully!
                     |""".stripMargin)
                  )
                )
            }
        }
    }
  }
}
