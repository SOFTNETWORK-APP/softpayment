package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.session.service.SessionMaterials
import org.softnetwork.session.model.JwtClaims

trait MockPaymentService extends MangoPayPaymentService with MockPaymentHandler {
  _: SessionMaterials[JwtClaims] =>
}
