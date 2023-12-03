package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.RefreshableHeaderSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableHeaderSessionSpec
    extends PaymentServiceSpec
    with RefreshableHeaderSessionServiceTestKit[JwtClaims]
    with PaymentRoutesTestKit
    with BasicSessionMaterials[JwtClaims]
