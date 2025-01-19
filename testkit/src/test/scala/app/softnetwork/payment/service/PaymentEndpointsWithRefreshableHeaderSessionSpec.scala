package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentEndpointsWithRefreshableHeaderSessionSpecTestKit,
  PaymentRouteSpec
}
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableHeaderSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentEndpointsWithRefreshableHeaderSessionSpecTestKit
