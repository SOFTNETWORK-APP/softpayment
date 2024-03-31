package app.softnetwork.payment.scalatest

import app.softnetwork.api.server.ApiRoutes
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import org.scalatest.Suite

trait SoftPayRouteTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentRouteTestKit[SD]
    with SoftPayTestKit {
  _: Suite with ApiRoutes with SessionMaterials[SD] =>
}
