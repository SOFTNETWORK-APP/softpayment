package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.{
  PaymentEndpointsWithRefreshableCookieSessionSpecTestKit,
  PaymentRouteSpec
}
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableCookieSessionSpec
    extends PaymentRouteSpec[JwtClaims]
    with PaymentEndpointsWithRefreshableCookieSessionSpecTestKit
