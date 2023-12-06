package app.softnetwork.payment.cli

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import app.softnetwork.payment.PaymentClientBuildInfo
import app.softnetwork.payment.cli.tokens.{Tokens, TokensConfig}
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.ExecutionContext

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    implicit def system: ActorSystem[_] = ActorSystem(Behaviors.empty, "PaymentClient")
    new Main().run(args)
  }
}

class Main() extends StrictLogging {
  val configs: List[CliConfig[_]] = List(
    TokensConfig
  )

  private def printUsage(): Unit = {
    // scalastyle:off println
    println(s"Payment Version ${PaymentClientBuildInfo.version}")
    println("Usage:")
    println("\tpayment [command]")
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

  def help(args: List[String]): Unit = {
    args match {
      case Nil | "help" :: Nil =>
        printUsage()
        System.exit(0)
      case "help" :: command :: any =>
        printUsage(command)
        System.exit(0)
      case _ =>
    }
  }

  def run(args: Array[String])(implicit system: ActorSystem[_]): Unit = {
    help(args.toList)
    args.toList match {
      case command :: any =>
        configs.find(_.command == command) match {
          case None =>
            println(s"ERROR: Unknown command --> $command")
            printUsage()
            System.exit(1)
          case Some(config) =>
            command match {
              case "tokens" =>
                TokensConfig.parse(any) match {
                  case None =>
                    println(s"ERROR: Invalid arguments for command --> $command")
                    printUsage(command)
                    System.exit(1)
                  case Some(conf) =>
                    implicit def ec: ExecutionContext = system.executionContext
                    Tokens.run(conf) onComplete { result =>
                      println(result)
                      System.exit(0)
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
