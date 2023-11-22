package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.OneOffHeaderSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials

class PaymentRoutesWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec
    with OneOffHeaderSessionServiceTestKit
    with PaymentRoutesTestKit
    with BasicSessionMaterials
