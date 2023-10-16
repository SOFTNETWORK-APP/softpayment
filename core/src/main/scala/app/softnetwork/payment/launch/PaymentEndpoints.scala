package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.{ApiEndpoints, Endpoint}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentEndpoints
import app.softnetwork.persistence.schema.SchemaProvider
import org.json4s.Formats

trait PaymentEndpoints extends ApiEndpoints { _: PaymentGuardian with SchemaProvider =>

  override implicit def formats: Formats = paymentFormats

  def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system =>
      List(
        paymentEndpoints(system)
      )
}
