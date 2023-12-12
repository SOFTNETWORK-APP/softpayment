package app.softnetwork.payment.cli.tokens

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.Client
import app.softnetwork.payment.cli.Cmd
import org.json4s.Formats
import scopt.OParser

import scala.concurrent.{ExecutionContext, Future}

object TokensCmd extends Cmd[TokensConfig] {

  val name: String = "tokens"

  val parser: OParser[Unit, TokensConfig] = {
    val builder = OParser.builder[TokensConfig]
    import builder._
    OParser.sequence(
      programName(s"$shell $name"),
      head(shell, name, "[options]"),
      opt[String]('r', "refreshToken")
        .action((x, c) => c.copy(refreshToken = Some(x)))
        .text("optional refresh token")
        .optional()
    )
  }

  def parse(args: Seq[String]): Option[TokensConfig] = {
    OParser.parse(parser, args, TokensConfig())
  }

  def run(
    config: TokensConfig
  )(implicit system: ActorSystem[_]): Future[(Int, Option[String])] = {
    implicit val ec: ExecutionContext = system.executionContext
    val client = Client(system)
    implicit val formats: Formats = org.json4s.DefaultFormats
    config.refreshToken match {
      case Some(refreshToken) =>
        client.refreshClientTokens(refreshToken) map {
          case Right(tokens) =>
            val json = org.json4s.jackson.Serialization.writePretty(tokens)
            (
              0,
              Some(s"""
               |Tokens refreshed successfully!
               |$json
               |""".stripMargin)
            )
          case Left(error) =>
            (
              1,
              Some(s"""
               |Tokens refresh failed!
               |$error
               |""".stripMargin)
            )
        }
      case None =>
        client.generateClientTokens() map {
          case Right(tokens) =>
            val json = org.json4s.jackson.Serialization.writePretty(tokens)
            (
              0,
              Some(s"""
               |Tokens generated successfully!
               |$json
               |""".stripMargin)
            )
          case Left(error) =>
            (
              1,
              Some(s"""
               |Tokens generation failed!
               |$error
               |""".stripMargin)
            )
        }
    }
  }

}
