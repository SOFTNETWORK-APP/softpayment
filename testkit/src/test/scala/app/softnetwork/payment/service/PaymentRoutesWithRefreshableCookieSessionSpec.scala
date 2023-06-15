package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.RefreshableCookieSessionServiceTestKit

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec
    with RefreshableCookieSessionServiceTestKit
    with PaymentRoutesTestKit
