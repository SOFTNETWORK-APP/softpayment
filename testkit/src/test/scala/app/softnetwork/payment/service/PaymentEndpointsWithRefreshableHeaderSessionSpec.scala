package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsWithRefreshableHeaderSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentEndpointsWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentEndpointsWithRefreshableHeaderSessionSpecTestKit
