package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.OneOffHeaderSessionServiceTestKit

class PaymentRoutesWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec
    with OneOffHeaderSessionServiceTestKit
    with PaymentRoutesTestKit
