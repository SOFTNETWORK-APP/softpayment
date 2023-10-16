package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MangoPayPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck

trait MangoPayEndpoints extends PaymentEndpoints {
  _: MangoPayApi with SchemaProvider with CsrfCheck =>
  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = system =>
    MangoPayPaymentEndpoints(system, sessionEndpoints(system))

  override def endpoints: ActorSystem[_] => List[Endpoint] = system =>
    super.endpoints(system) :+ paymentSwagger(system)
}
