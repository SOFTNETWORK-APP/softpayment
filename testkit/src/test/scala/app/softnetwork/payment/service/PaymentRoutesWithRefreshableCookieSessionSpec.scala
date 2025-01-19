package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentRouteSpec,
  PaymentRoutesWithRefreshableCookieSessionSpecTestKit
}
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentRoutesWithRefreshableCookieSessionSpecTestKit
