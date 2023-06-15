package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MockPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.{
  SessionEndpointsRoute,
  SessionEndpointsRoutes,
  SessionServiceRoute
}
import com.softwaremill.session.CsrfCheck
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait PaymentEndpointsTestKit extends PaymentEndpoints { _: SchemaProvider =>

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = system =>
    MockPaymentEndpoints(system, sessionEndpoints(system))

  override def endpoints
    : ActorSystem[_] => List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    system =>
      SessionEndpointsRoute(system, sessionEndpoints(system)).endpoints ++ paymentEndpoints(
        system
      ).endpoints

}
