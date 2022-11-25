package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route

trait MangoPayServiceRoutes extends GenericPaymentServiceRoutes {
  override def apiRoutes(system: ActorSystem[_]): Route = MangoPayPaymentService(system).route
}
