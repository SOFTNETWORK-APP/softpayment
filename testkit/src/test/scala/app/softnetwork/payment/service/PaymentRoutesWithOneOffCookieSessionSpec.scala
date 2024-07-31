package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesWithOneOffCookieSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentRoutesWithOneOffCookieSessionSpecTestKit
