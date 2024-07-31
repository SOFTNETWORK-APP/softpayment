package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsWithRefreshableCookieSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentEndpointsWithRefreshableCookieSessionSpecTestKit
