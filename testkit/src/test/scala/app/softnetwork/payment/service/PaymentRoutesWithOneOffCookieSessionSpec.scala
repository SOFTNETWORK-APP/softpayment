package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials

class PaymentRoutesWithOneOffCookieSessionSpec
    extends PaymentServiceSpec
    with OneOffCookieSessionServiceTestKit
    with PaymentRoutesTestKit
    with BasicSessionMaterials
