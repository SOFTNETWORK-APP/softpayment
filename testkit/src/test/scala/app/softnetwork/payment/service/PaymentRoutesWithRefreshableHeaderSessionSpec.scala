package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentRouteSpec,
  PaymentRoutesWithRefreshableHeaderSessionSpecTestKit
}
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableHeaderSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentRoutesWithRefreshableHeaderSessionSpecTestKit
