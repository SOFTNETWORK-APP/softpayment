package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.scalatest.OneOffHeaderSessionServiceTestKit
import app.softnetwork.session.service.JwtClaimsSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with OneOffHeaderSessionServiceTestKit[JwtClaims]
    with PaymentRoutesTestKit[JwtClaims]
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
