package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.RefreshableCookieSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec
    with RefreshableCookieSessionServiceTestKit
    with PaymentRoutesTestKit
    with BasicSessionMaterials
