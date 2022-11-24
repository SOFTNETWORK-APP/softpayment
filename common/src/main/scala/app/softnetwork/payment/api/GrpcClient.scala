package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem

import scala.concurrent.ExecutionContext

trait GrpcClient {

  implicit def system: ActorSystem[_]
  implicit lazy val ec: ExecutionContext = system.executionContext

  def name: String

}
