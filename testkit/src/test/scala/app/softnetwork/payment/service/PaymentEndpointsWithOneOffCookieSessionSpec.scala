package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsWithOneOffCookieSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentEndpointsWithOneOffCookieSessionSpecTestKit
