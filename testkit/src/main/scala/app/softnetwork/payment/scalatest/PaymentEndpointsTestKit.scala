package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiEndpoint
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MockPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.SessionEndpointsRoute

trait PaymentEndpointsTestKit extends PaymentEndpoints { _: SchemaProvider =>

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = system =>
    MockPaymentEndpoints(system, sessionEndpoints(system))

  override def endpoints: ActorSystem[_] => List[ApiEndpoint] =
    system =>
      List(
        SessionEndpointsRoute(system, sessionEndpoints(system)),
        paymentEndpoints(system)
      )

}
