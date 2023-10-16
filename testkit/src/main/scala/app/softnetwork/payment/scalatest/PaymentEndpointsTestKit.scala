package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.launch.{PaymentEndpoints, PaymentGuardian}
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MockPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.SessionEndpointsRoutes
import app.softnetwork.session.CsrfCheck

trait PaymentEndpointsTestKit extends PaymentEndpoints with SessionEndpointsRoutes {
  _: PaymentGuardian with SchemaProvider with CsrfCheck =>

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = system =>
    MockPaymentEndpoints(system, sessionEndpoints(system))

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system =>
      List(
        sessionServiceEndpoints(system),
        paymentEndpoints(system),
        MockPaymentEndpoints.swagger(system, sessionEndpoints(system))
      )

}
