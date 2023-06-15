package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.RefreshableHeaderSessionServiceTestKit

class PaymentRoutesWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec
    with RefreshableHeaderSessionServiceTestKit
    with PaymentRoutesTestKit
