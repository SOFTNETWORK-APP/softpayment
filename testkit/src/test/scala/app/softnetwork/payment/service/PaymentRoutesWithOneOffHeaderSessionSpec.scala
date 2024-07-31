package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesWithOneOffHeaderSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentRoutesWithOneOffHeaderSessionSpecTestKit
