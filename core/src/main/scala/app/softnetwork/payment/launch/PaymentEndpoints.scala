package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiEndpoints
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentEndpoints
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.service.SessionEndpoints
import org.json4s.Formats
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait PaymentEndpoints extends ApiEndpoints with PaymentGuardian { _: SchemaProvider =>

  override implicit def formats: Formats = paymentFormats

  def sessionEndpoints: ActorSystem[_] => SessionEndpoints

  def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints

  override def endpoints
    : ActorSystem[_] => List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    system => paymentEndpoints(system).endpoints
}
