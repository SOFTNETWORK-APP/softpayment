package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{GenericPaymentService, MangoPayPaymentService}
import app.softnetwork.persistence.schema.SchemaProvider

trait MangoPayRoutes extends PaymentRoutes { _: SchemaProvider =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = system =>
    MangoPayPaymentService(system, sessionService(system))

}
