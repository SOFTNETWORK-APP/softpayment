package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesWithRefreshableHeaderSessionSpecTestKit
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec[JwtClaims]
    with PaymentRoutesWithRefreshableHeaderSessionSpecTestKit
