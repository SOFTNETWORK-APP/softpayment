package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiEndpoint
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MockPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.SessionEndpointsRoutes
import com.softwaremill.session.CsrfCheck

trait PaymentEndpointsTestKit extends PaymentEndpoints with SessionEndpointsRoutes {
  _: SchemaProvider with CsrfCheck =>

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = system =>
    MockPaymentEndpoints(system, sessionEndpoints(system))

  override def endpoints: ActorSystem[_] => List[ApiEndpoint] =
    system =>
      List(
        sessionServiceEndpoints(system),
        paymentEndpoints(system)
      )

}
