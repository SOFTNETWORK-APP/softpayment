package app.softnetwork.payment.cli.tokens

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.Client
import app.softnetwork.payment.cli.Command
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

object Tokens extends Command[TokensConfig] with StrictLogging {
  def run(
    config: TokensConfig
  )(implicit system: ActorSystem[_]): Future[String] = {
    implicit val ec: ExecutionContext = system.executionContext
    val client = Client(system)
    config.refreshToken match {
      case Some(refreshToken) =>
        client.refreshClientTokens(refreshToken) map {
          case Some(tokens) =>
            s"""
                 |Tokens refreshed successfully!
                 |${tokens.toString}
                 |""".stripMargin
          case _ =>
            s"""
                 |Tokens refresh failed!
                 |""".stripMargin
        }
      case None =>
        client.generateClientTokens() map {
          case Some(tokens) =>
            s"Tokens generated successfully! ${tokens.toString}"
          case _ =>
            "Tokens generation failed!"
        }
    }
  }

}
