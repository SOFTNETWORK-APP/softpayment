package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.{ApiEndpoint, ApiEndpoints}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentEndpoints
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.service.SessionEndpoints
import org.json4s.Formats

trait PaymentEndpoints extends ApiEndpoints with PaymentGuardian { _: SchemaProvider =>

  override implicit def formats: Formats = paymentFormats

  def sessionEndpoints: ActorSystem[_] => SessionEndpoints

  def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints

  override def endpoints: ActorSystem[_] => List[ApiEndpoint] =
    system =>
      List(
        paymentEndpoints(system)
      )
}
