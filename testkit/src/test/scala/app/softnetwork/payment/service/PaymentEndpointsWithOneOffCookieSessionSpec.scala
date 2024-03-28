package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.handlers.SessionRefreshTokenDao
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.service.BasicSessionMaterials
import com.softwaremill.session.RefreshTokenStorage
import org.softnetwork.session.model.Session

class PaymentEndpointsWithOneOffCookieSessionSpec
    extends PaymentServiceSpec[Session]
    with OneOffCookieSessionEndpointsTestKit[Session]
    with PaymentEndpointsTestKit[Session]
    with CsrfCheckHeader
    with BasicSessionMaterials[Session] {
  override implicit def companion: SessionDataCompanion[Session] = Session

  override implicit def refreshTokenStorage: RefreshTokenStorage[Session] = SessionRefreshTokenDao(
    ts
  )
}
