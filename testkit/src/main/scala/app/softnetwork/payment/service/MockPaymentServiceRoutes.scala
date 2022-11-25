package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.api.{MockPaymentServer, PaymentServiceApiHandler}

import scala.concurrent.Future

trait MockPaymentServiceRoutes extends GenericPaymentServiceRoutes {
  override def grpcServices
    : ActorSystem[_] => Seq[PartialFunction[HttpRequest, Future[HttpResponse]]] =
    system =>
      Seq(
        PaymentServiceApiHandler.partial(MockPaymentServer(system))(system)
      )
  override def apiRoutes(system: ActorSystem[_]): Route = MockPaymentService(system).route
}
