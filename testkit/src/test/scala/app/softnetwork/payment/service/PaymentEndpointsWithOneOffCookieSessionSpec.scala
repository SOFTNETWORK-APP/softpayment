package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffCookieSessionSpec
    extends PaymentServiceSpec
    with OneOffCookieSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
    with BasicSessionMaterials[JwtClaims]
