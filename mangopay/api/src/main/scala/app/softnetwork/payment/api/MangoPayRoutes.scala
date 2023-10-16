package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{GenericPaymentService, MangoPayPaymentService}
import app.softnetwork.persistence.schema.SchemaProvider

trait MangoPayRoutes extends PaymentRoutes { _: MangoPayApi with SchemaProvider =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = system =>
    MangoPayPaymentService(system, sessionService(system))

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] = system =>
    super.apiRoutes(system) :+ paymentSwagger(system)
}
