package app.softnetwork.payment.cli

import akka.actor.typed.ActorSystem

import scala.concurrent.Future

trait Command[T] {

  def run(config: T)(implicit system: ActorSystem[_]): Future[String]

}
