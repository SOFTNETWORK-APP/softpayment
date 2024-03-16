package app.softnetwork.payment.cli

import akka.actor.typed.ActorSystem
import scopt.OParser

import scala.concurrent.Future

trait Cmd[T] {

  def name: String

  private[cli] def shell: String = Main.shell

  def parser: OParser[Unit, T]

  private[cli] def usage(): String = OParser.usage(parser)

  def parse(args: Seq[String]): Option[T]

  final def run(
    args: Seq[String]
  )(implicit system: ActorSystem[_]): Future[(Int, Option[String])] = {
    parse(args) match {
      case Some(config) => run(config)
      case None =>
        Future.successful(
          (1, Some(s"ERROR: Invalid arguments for command --> $shell $name\n${usage()}"))
        )
    }
  }

  def run(config: T)(implicit system: ActorSystem[_]): Future[(Int, Option[String])]

}
