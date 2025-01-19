package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentRouteSpec,
  PaymentRoutesWithOneOffHeaderSessionSpecTestKit
}
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffHeaderSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentRoutesWithOneOffHeaderSessionSpecTestKit
