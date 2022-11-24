package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route

trait MockPaymentServiceRoutes extends GenericPaymentServiceRoutes {
  override def apiRoutes(system: ActorSystem[_]): Route = MockPaymentService(system).route
}
