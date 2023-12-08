package app.softnetwork.payment.cli.tokens

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.Client
import app.softnetwork.payment.cli.Command
import com.typesafe.scalalogging.StrictLogging
import org.json4s.Formats

import scala.concurrent.{ExecutionContext, Future}

object Tokens extends Command[TokensConfig] with StrictLogging {
  def run(
    config: TokensConfig
  )(implicit system: ActorSystem[_]): Future[String] = {
    implicit val ec: ExecutionContext = system.executionContext
    val client = Client(system)
    implicit val formats: Formats = org.json4s.DefaultFormats
    config.refreshToken match {
      case Some(refreshToken) =>
        client.refreshClientTokens(refreshToken) map {
          case Right(tokens) =>
            val json = org.json4s.jackson.Serialization.writePretty(tokens)
            s"""
                 |Tokens refreshed successfully!
                 |$json
                 |""".stripMargin
          case Left(error) =>
            s"""
                 |Tokens refresh failed!
                 |$error
                 |""".stripMargin
        }
      case None =>
        client.generateClientTokens() map {
          case Right(tokens) =>
            val json = org.json4s.jackson.Serialization.writePretty(tokens)
            s"""
               |Tokens generated successfully!
               |$json
               |""".stripMargin
          case Left(error) =>
            s"""
               |Tokens generation failed!
               |$error
               |""".stripMargin
        }
    }
  }

}
