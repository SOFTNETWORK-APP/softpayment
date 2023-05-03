package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route
import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentService
import org.json4s.Formats

trait PaymentRoutes extends ApiRoutes with PaymentGuardian {

  override implicit def formats: Formats = paymentFormats

  def paymentService: ActorSystem[_] => GenericPaymentService

  override def apiRoutes(system: ActorSystem[_]): Route = paymentService(system).route

}
