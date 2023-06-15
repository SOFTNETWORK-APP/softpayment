package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionServiceTestKit

class PaymentRoutesWithOneOffCookieSessionSpec
    extends PaymentServiceSpec
    with OneOffCookieSessionServiceTestKit
    with PaymentRoutesTestKit
