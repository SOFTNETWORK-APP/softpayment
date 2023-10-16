package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.launch.{PaymentGuardian, PaymentRoutes}
import app.softnetwork.payment.service.{GenericPaymentService, MockPaymentService}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.SessionServiceRoutes

trait PaymentRoutesTestKit extends PaymentRoutes with SessionServiceRoutes {
  _: PaymentGuardian with SchemaProvider =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = system =>
    MockPaymentService(system, sessionService(system))

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system =>
      List(
        sessionServiceRoute(system),
        paymentService(system)
      )

}
