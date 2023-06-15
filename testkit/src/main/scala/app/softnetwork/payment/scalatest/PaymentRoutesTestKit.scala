package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{GenericPaymentService, MockPaymentService}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.SessionServiceRoute

trait PaymentRoutesTestKit extends PaymentRoutes { _: SchemaProvider =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = system =>
    MockPaymentService(system, sessionService(system))

  override def apiRoutes(system: ActorSystem[_]): Route =
    paymentService(system).route ~
    SessionServiceRoute(sessionService(system)).route

}
