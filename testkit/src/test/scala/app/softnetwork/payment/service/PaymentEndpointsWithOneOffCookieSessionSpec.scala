package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionEndpointsTestKit
import com.softwaremill.session.CsrfCheckHeader

class PaymentEndpointsWithOneOffCookieSessionSpec
    extends PaymentServiceSpec
    with OneOffCookieSessionEndpointsTestKit
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
