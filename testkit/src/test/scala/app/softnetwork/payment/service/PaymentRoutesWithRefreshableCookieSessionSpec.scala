package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.scalatest.RefreshableCookieSessionServiceTestKit
import app.softnetwork.session.service.JwtClaimsSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with RefreshableCookieSessionServiceTestKit[JwtClaims]
    with PaymentRoutesTestKit[JwtClaims]
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
