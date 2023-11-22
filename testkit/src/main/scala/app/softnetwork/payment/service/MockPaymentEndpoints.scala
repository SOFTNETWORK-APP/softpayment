package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.session.service.SessionMaterials

trait MockPaymentEndpoints extends MangoPayPaymentEndpoints with MockPaymentHandler {
  _: SessionMaterials =>
}
