package app.softnetwork.payment.service

import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.message.PaymentMessages.{PaymentCommand, PaymentResult}
import app.softnetwork.persistence.typed.scaladsl.EntityPattern
import sttp.tapir.{Endpoint, Tapir}
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.server.ServerEndpoint.Full

import scala.concurrent.Future

trait HooksEndpoints extends Tapir with SchemaDerivation with BasicPaymentService with Completion {
  _: EntityPattern[PaymentCommand, PaymentResult] =>

  def hooks(
    rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any]
  ): Full[Unit, Unit, _, Unit, Unit, Any, Future]

}
