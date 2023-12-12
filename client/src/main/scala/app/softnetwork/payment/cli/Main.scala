package app.softnetwork.payment.cli

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.SoftpayBuildInfo
import app.softnetwork.payment.cli.activate.ActivateClientCmd
import app.softnetwork.payment.cli.clients.ClientsCmd
import app.softnetwork.payment.cli.signup.SignUpClientCmd
import app.softnetwork.payment.cli.tokens.TokensCmd
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
  private[cli] val cmds: List[Cmd[_]] = List(
    SignUpClientCmd,
    ActivateClientCmd,
    TokensCmd,
    ClientsCmd
  )

  private[cli] def printUsage(): Unit = {
    // scalastyle:off println
    println(s"Softpay Version ${SoftpayBuildInfo.version}")
    println("Usage:")
    println(s"\t${Main.shell} [command]")
    println("Available commands =>")
    cmds.foreach { cmd =>
      println(s"\t${cmd.name}")
    }
  }

  private[cli] def printUsage(cmd: String): Unit = {
    // scalastyle:off println
    cmds.find(_.name == cmd) match {
      case None =>
        println(s"ERROR: Unknown command --> $cmd")
      case Some(cmd) =>
        println(cmd.usage())
    }
  }
  // scalastyle:on println

  private[cli] def help(args: List[String]): Unit = {
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
        cmds.find(_.name == command) match {
          case None =>
            println(s"ERROR: Unknown command --> $command")
            printUsage()
            System.exit(1)
          case Some(cmd) =>
            cmd.run(list) complete () match {
              case Success((exit, message)) =>
                message.foreach(println)
                System.exit(exit)
              case Failure(f) =>
                logger.error(s"Failed to run command ${cmd.name}", f)
                System.exit(1)
            }
        }
      case _ =>
        printUsage()
        System.exit(1)
    }
  }
}
