package app.softnetwork.payment.api

import akka.actor.CoordinatedShutdown
import akka.actor.typed.ActorSystem
import akka.grpc.scaladsl.AkkaGrpcClient

import scala.concurrent.ExecutionContext

trait GrpcClient {

  implicit def system: ActorSystem[_]
  implicit lazy val ec: ExecutionContext = system.executionContext

  def name: String

  def akkaGrpcClient: AkkaGrpcClient
}

trait GrpcClientFactory[T <: GrpcClient] {
  def name: String
  private[this] var client: Option[T] = None
  def init(sys: ActorSystem[_]): T
  def apply(sys: ActorSystem[_]): T = {
    client match {
      case Some(value) => value
      case _ =>
        import app.softnetwork.persistence.typed._
        implicit val classicSystem: _root_.akka.actor.ActorSystem = sys
        val shutdown = CoordinatedShutdown(classicSystem)
        val cli = init(sys)
        client = Some(cli)
        shutdown.addTask(
          CoordinatedShutdown.PhaseServiceRequestsDone,
          s"$name-graceful-terminate"
        ) { () =>
          client = None
          cli.akkaGrpcClient.close()
        }
        cli
    }
  }
}
