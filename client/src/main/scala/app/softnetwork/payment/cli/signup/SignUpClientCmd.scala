package app.softnetwork.payment.cli.signup

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.api.{Client, ProviderType}
import app.softnetwork.payment.cli.Cmd
import org.json4s.Formats
import scopt.OParser

import scala.concurrent.{ExecutionContext, Future}

object SignUpClientCmd extends Cmd[SignUpClientConfig] {

  val name: String = "signup"

  val parser: OParser[Unit, SignUpClientConfig] = {
    val builder = OParser.builder[SignUpClientConfig]
    import builder._
    OParser.sequence(
      programName(s"$shell $name"),
      head(shell, name, "[options]"),
      opt[String]('p', "principal")
        .action((x, c) => c.copy(principal = x))
        .text("principal")
        .required(),
      opt[String]('c', "credentials")
        .action((x, c) => c.copy(credentials = x.toCharArray))
        .text("credentials")
        .required()
      /*.validate(x =>
          import app.softnetwork.account.config.AccountSettings.passwordRules
          passwordRules().validate(x) match {
            case Right(_)    => success
            case Left(error) => failure(error.mkString(", "))
          }
        )*/,
      opt[String]('i', "providerId")
        .action((x, c) => c.copy(providerId = x))
        .text("payment provider Id")
        .required(),
      opt[String]('k', "providerApiKey")
        .action((x, c) => c.copy(providerApiKey = x.toCharArray))
        .text("payment provider Api Key")
        .required(),
      opt[String]('t', "providerType")
        .action((x, c) => c.copy(providerType = ProviderType.fromName(x.toUpperCase)))
        .text("optional payment provider type - default is 'MangoPay'")
        .optional()
    )
  }

  def parse(args: Seq[String]): Option[SignUpClientConfig] = {
    OParser.parse(parser, args, SignUpClientConfig())
  }

  override def run(
    config: SignUpClientConfig
  )(implicit system: ActorSystem[_]): Future[(Int, Option[String])] = {
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
        SoftPayClientSettings(client.clientId, client.clientSecret).write()
        val json = org.json4s.jackson.Serialization.writePretty(client)
        (
          0,
          Some(s"""
           |Client registered successfully!
           |$json
           |""".stripMargin)
        )
      case Left(error) =>
        (
          1,
          Some(s"""
           |Client registration failed!
           |$error
           |""".stripMargin)
        )
    }
  }
}
