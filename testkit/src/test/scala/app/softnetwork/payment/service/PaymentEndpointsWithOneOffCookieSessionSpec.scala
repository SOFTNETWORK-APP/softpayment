package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.service.JwtClaimsSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with OneOffCookieSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
