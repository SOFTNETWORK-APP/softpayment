package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.{ApiRoute, ApiRoutes}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentService
import app.softnetwork.persistence.schema.SchemaProvider
import org.json4s.Formats

trait PaymentRoutes extends ApiRoutes { _: PaymentGuardian with SchemaProvider =>

  override implicit def formats: Formats = paymentFormats

  def paymentService: ActorSystem[_] => GenericPaymentService

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system =>
      List(
        paymentService(system)
      )

}
