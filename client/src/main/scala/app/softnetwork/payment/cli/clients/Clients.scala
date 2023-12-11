package app.softnetwork.payment.cli.clients

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.config.{ApiKeys, SoftPayClientSettings}
import app.softnetwork.payment.cli.Command

import scala.concurrent.Future

object Clients extends Command[ClientsConfig] {
  override def run(config: ClientsConfig)(implicit system: ActorSystem[_]): Future[String] = {
    config.subCommand match {
      case ClientsSubCommand.List =>
        Future.successful(
          s"""
             |Available clients:
             |\t${ApiKeys.list().filter(_._2.trim.nonEmpty).keys.mkString("\n\t")}
             |""".stripMargin
        )
      case ClientsSubCommand.Set =>
        config.clientId match {
          case Some(clientId) =>
            Future.successful(SoftPayClientSettings.select(clientId) match {
              case Some(softPayClientSettings) =>
                s"""
                   |Client $clientId selected successfully!
                   |\tapiKey: ${softPayClientSettings.apiKey}
                   |""".stripMargin
              case None =>
                s"""
                   |apiKey not found for $clientId!
                   |""".stripMargin
            })
        }
      case ClientsSubCommand.Get =>
        config.clientId match {
          case Some(clientId) =>
            Future.successful(ApiKeys.get(clientId) match {
              case Some(apiKey) =>
                s"""
                     |Client $clientId loaded successfully!
                     |\tapiKey: $apiKey
                     |""".stripMargin
              case None =>
                s"""
                     |apiKey not found for $clientId!
                     |""".stripMargin
            })
        }
      case ClientsSubCommand.Add =>
        config.clientId match {
          case Some(clientId) =>
            ApiKeys.+(clientId, config.apiKey.getOrElse("")) match {
              case _ =>
                Future.successful(
                  s"""
                       |Client $clientId added successfully!
                       |""".stripMargin
                )
            }
        }
      case ClientsSubCommand.Remove =>
        config.clientId match {
          case Some(clientId) =>
            ApiKeys.-(clientId) match {
              case _ =>
                Future.successful(
                  s"""
                         |Client $clientId removed successfully!
                         |""".stripMargin
                )
            }
        }
    }
  }
}
