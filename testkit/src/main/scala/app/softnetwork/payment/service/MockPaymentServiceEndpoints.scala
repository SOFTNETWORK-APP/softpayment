package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials

trait MockPaymentServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends MangoPayPaymentServiceEndpoints[SD]
    with MockPaymentHandler {
  _: SessionMaterials[SD] =>
}
