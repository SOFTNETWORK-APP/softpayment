package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.RefreshableCookieSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.service.JwtClaimsSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with RefreshableCookieSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
