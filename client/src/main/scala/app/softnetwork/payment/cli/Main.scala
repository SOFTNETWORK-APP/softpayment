package app.softnetwork.payment.cli

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.SoftpayBuildInfo
import app.softnetwork.payment.cli.activate.{ActivateClient, ActivateClientConfig}
import app.softnetwork.payment.cli.clients.{Clients, ClientsConfig}
import app.softnetwork.payment.cli.signup.{SignUpClient, SignUpClientConfig}
import app.softnetwork.payment.cli.tokens.{Tokens, TokensConfig}
import com.typesafe.scalalogging.StrictLogging

import scala.util.{Failure, Success}

object Main extends StrictLogging {

  def shell: String = "softpay"

  def main(args: Array[String]): Unit = {
    implicit def system: ActorSystem[_] = ActorSystem(Behaviors.empty, "PaymentClient")
    new Main().run(args)
  }
}

class Main extends Completion with StrictLogging {
  val configs: List[CliConfig[_]] = List(
    SignUpClientConfig,
    ActivateClientConfig,
    TokensConfig,
    ClientsConfig
  )

  private def printUsage(): Unit = {
    // scalastyle:off println
    println(s"Softpay Version ${SoftpayBuildInfo.version}")
    println("Usage:")
    println(s"\t${Main.shell} [command]")
    println("Available commands =>")
    configs.foreach { config =>
      println(s"\t${config.command}")
    }
  }

  private def printUsage(command: String): Unit = {
    // scalastyle:off println
    configs.find(_.command == command) match {
      case None =>
        println(s"ERROR: Unknown command --> $command")
      case Some(config) =>
        println(config.usage())
    }
  }
  // scalastyle:on println

  private def help(args: List[String]): Unit = {
    args match {
      case Nil | "help" :: Nil =>
        printUsage()
        System.exit(0)
      case "help" :: command :: _ =>
        printUsage(command)
        System.exit(0)
      case command :: "help" :: Nil =>
        printUsage(command)
        System.exit(0)
      case _ =>
    }
  }

  def run(args: Array[String])(implicit system: ActorSystem[_]): Unit = {
    help(args.toList)
    args.toList match {
      case command :: list =>
        configs.find(_.command == command) match {
          case None =>
            println(s"ERROR: Unknown command --> $command")
            printUsage()
            System.exit(1)
          case Some(config) =>
            command match {
              case SignUpClientConfig.command =>
                SignUpClientConfig.parse(list) match {
                  case None =>
                    println(s"ERROR: Invalid arguments for command --> $command")
                    printUsage(config.usage())
                    System.exit(1)
                  case Some(conf) =>
                    SignUpClient.run(conf) complete () match {
                      case Success(result) =>
                        println(result)
                        System.exit(0)
                      case Failure(f) =>
                        logger.error(s"Failed to run command $command", f)
                        System.exit(1)
                    }
                }
              case TokensConfig.command =>
                TokensConfig.parse(list) match {
                  case None =>
                    println(s"ERROR: Invalid arguments for command --> $command")
                    printUsage(config.usage())
                    System.exit(1)
                  case Some(conf) =>
                    Tokens.run(conf) complete () match {
                      case Success(result) =>
                        println(result)
                        System.exit(0)
                      case Failure(f) =>
                        logger.error(s"Failed to run command $command", f)
                        System.exit(1)
                    }
                }
              case ActivateClientConfig.command =>
                ActivateClientConfig.parse(list) match {
                  case None =>
                    println(s"ERROR: Invalid arguments for command --> $command")
                    printUsage(config.usage())
                    System.exit(1)
                  case Some(conf) =>
                    ActivateClient.run(conf) complete () match {
                      case Success(result) =>
                        println(result)
                        System.exit(0)
                      case Failure(f) =>
                        logger.error(s"Failed to run command $command", f)
                        System.exit(1)
                    }
                }
              case ClientsConfig.command =>
                ClientsConfig.parse(list) match {
                  case None =>
                    println(s"ERROR: Invalid arguments for command --> $command")
                    printUsage(config.usage())
                    System.exit(1)
                  case Some(conf) =>
                    Clients.run(conf) complete () match {
                      case Success(result) =>
                        println(result)
                        System.exit(0)
                      case Failure(f) =>
                        logger.error(s"Failed to run command $command", f)
                        System.exit(1)
                    }
                }
              case _ =>
                println(s"ERROR: Unknown command --> $command")
                printUsage()
                System.exit(1)
            }
        }
      case _ =>
        printUsage()
        System.exit(1)
    }
  }
}
