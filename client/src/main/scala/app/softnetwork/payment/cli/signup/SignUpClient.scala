package app.softnetwork.payment.cli.signup

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.Client
import app.softnetwork.payment.cli.Command
import org.json4s.Formats

import scala.concurrent.{ExecutionContext, Future}

object SignUpClient extends Command[SignUpClientConfig] {
  override def run(config: SignUpClientConfig)(implicit system: ActorSystem[_]): Future[String] = {
    implicit val ec: ExecutionContext = system.executionContext
    val client = Client(system)
    implicit val formats: Formats = org.json4s.DefaultFormats
    client.signUpClient(
      config.principal,
      config.credentials,
      config.providerId,
      config.providerApiKey,
      config.providerType
    ) map {
      case Right(client) =>
        val json = org.json4s.jackson.Serialization.writePretty(client)
        s"""
           |Client registered successfully!
           |$json
           |""".stripMargin
      case Left(error) =>
        s"""
           |Client registration failed!
           |$error
           |""".stripMargin
    }
  }
}
