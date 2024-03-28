package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import app.softnetwork.api.server.GrpcService

import scala.concurrent.Future

class PaymentGrpcService(server: PaymentServer) extends GrpcService {

  override def grpcService: ActorSystem[_] => PartialFunction[HttpRequest, Future[HttpResponse]] =
    system => PaymentServiceApiHandler.partial(server)(system)

}
