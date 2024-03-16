package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.RefreshableHeaderSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.service.JwtClaimsSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with RefreshableHeaderSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
