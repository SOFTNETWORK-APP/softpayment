package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.RefreshableHeaderSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials

class PaymentRoutesWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec
    with RefreshableHeaderSessionServiceTestKit
    with PaymentRoutesTestKit
    with BasicSessionMaterials
