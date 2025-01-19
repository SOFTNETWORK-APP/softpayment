package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentEndpointsWithOneOffHeaderSessionSpecTestKit,
  PaymentRouteSpec
}
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithOneOffHeaderSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentEndpointsWithOneOffHeaderSessionSpecTestKit
