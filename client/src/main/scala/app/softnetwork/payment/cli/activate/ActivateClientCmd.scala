package app.softnetwork.payment.cli.activate

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.Client
import app.softnetwork.payment.cli.Cmd
import scopt.OParser

import scala.concurrent.{ExecutionContext, Future}

object ActivateClientCmd extends Cmd[ActivateClientConfig] {

  val name: String = "activate"

  val parser: OParser[Unit, ActivateClientConfig] = {
    val builder = OParser.builder[ActivateClientConfig]
    import builder._
    OParser.sequence(
      programName(s"$shell $name"),
      head(shell, name, "[options]"),
      opt[String]('t', "token")
        .action((x, c) => c.copy(token = x))
        .text("token")
        .required()
    )
  }

  def parse(args: Seq[String]): Option[ActivateClientConfig] = {
    OParser.parse(parser, args, ActivateClientConfig())
  }

  override def run(
    config: ActivateClientConfig
  )(implicit system: ActorSystem[_]): Future[(Int, Option[String])] = {
    implicit val ec: ExecutionContext = system.executionContext
    val client = Client(system)
    client.activateClient(config.token) map {
      case Right(activated) =>
        if (activated) (0, Some("Client activated successfully!"))
        else (1, Some("Client activation failed!"))
      case Left(error) =>
        (
          1,
          Some(s"""
           |Client activation failed!
           |$error
           |""".stripMargin)
        )
    }
  }
}
