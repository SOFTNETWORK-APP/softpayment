package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentEndpointsTestKit
import app.softnetwork.session.scalatest.RefreshableHeaderSessionEndpointsTestKit
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.service.BasicSessionMaterials

class PaymentEndpointsWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec
    with RefreshableHeaderSessionEndpointsTestKit
    with PaymentEndpointsTestKit
    with CsrfCheckHeader
    with BasicSessionMaterials
