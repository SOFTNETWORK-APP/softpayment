package app.softnetwork.payment.cli.activate

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.Client
import app.softnetwork.payment.cli.Command

import scala.concurrent.{ExecutionContext, Future}

object ActivateClient extends Command[ActivateClientConfig] {
  override def run(
    config: ActivateClientConfig
  )(implicit system: ActorSystem[_]): Future[String] = {
    implicit val ec: ExecutionContext = system.executionContext
    val client = Client(system)
    client.activateClient(config.token) map {
      case Right(activated) =>
        if (activated) "Client activated successfully!"
        else "Client activation failed!"
      case Left(error) =>
        s"""
           |Client activation failed!
           |$error
           |""".stripMargin
    }
  }
}
