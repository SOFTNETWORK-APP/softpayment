package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.RefreshableCookieSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader

class PaymentEndpointsWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec
    with RefreshableCookieSessionEndpointsTestKit
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
