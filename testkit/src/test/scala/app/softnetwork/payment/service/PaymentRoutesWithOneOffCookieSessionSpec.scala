package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentRouteSpec,
  PaymentRoutesWithOneOffCookieSessionSpecTestKit
}
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffCookieSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentRoutesWithOneOffCookieSessionSpecTestKit
