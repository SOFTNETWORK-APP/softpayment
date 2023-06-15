package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MangoPayPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import com.softwaremill.session.CsrfCheck

trait MangoPayEndpoints extends PaymentEndpoints { _: SchemaProvider with CsrfCheck =>
  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = system =>
    MangoPayPaymentEndpoints(system, sessionEndpoints(system))

}
