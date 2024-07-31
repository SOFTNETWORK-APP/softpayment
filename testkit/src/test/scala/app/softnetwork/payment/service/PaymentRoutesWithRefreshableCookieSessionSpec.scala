package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesWithRefreshableCookieSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentRoutesWithRefreshableCookieSessionSpecTestKit
