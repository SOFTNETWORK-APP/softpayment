package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.OneOffHeaderSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec
    with OneOffHeaderSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
    with BasicSessionMaterials[JwtClaims]
