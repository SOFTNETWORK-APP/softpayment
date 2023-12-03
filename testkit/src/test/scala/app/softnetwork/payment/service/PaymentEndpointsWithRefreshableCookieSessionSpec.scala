package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.RefreshableCookieSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec
    with RefreshableCookieSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
    with BasicSessionMaterials[JwtClaims]
