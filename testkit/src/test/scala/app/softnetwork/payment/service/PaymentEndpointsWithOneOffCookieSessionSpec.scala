package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentEndpointsWithOneOffCookieSessionSpecTestKit,
  PaymentRouteSpec
}
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffCookieSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentEndpointsWithOneOffCookieSessionSpecTestKit
