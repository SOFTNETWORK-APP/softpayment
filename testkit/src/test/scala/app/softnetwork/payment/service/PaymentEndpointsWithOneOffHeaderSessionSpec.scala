package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsWithOneOffHeaderSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentEndpointsWithOneOffHeaderSessionSpecTestKit
