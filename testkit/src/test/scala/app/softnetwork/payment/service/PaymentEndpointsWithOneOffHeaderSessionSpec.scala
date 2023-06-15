package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.OneOffHeaderSessionEndpointsTestKit
import com.softwaremill.session.CsrfCheckHeader

class PaymentEndpointsWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec
    with OneOffHeaderSessionEndpointsTestKit
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
