package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentService
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.service.SessionService
import org.json4s.Formats

trait PaymentRoutes extends ApiRoutes with PaymentGuardian { _: SchemaProvider =>

  override implicit def formats: Formats = paymentFormats

  def sessionService: ActorSystem[_] => SessionService

  def paymentService: ActorSystem[_] => GenericPaymentService

  override def apiRoutes(system: ActorSystem[_]): Route = paymentService(system).route

}
