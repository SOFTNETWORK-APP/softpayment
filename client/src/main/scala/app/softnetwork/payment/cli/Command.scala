package app.softnetwork.payment.cli

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.PaymentClient

import scala.concurrent.{ExecutionContext, Future}

trait Command[T] {

  def run(config: T)(implicit system: ActorSystem[_]): Future[String]

}
