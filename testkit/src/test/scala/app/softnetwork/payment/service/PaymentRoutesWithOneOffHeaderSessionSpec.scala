package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.OneOffHeaderSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffHeaderSessionSpec
    extends PaymentServiceSpec
    with OneOffHeaderSessionServiceTestKit[JwtClaims]
    with PaymentRoutesTestKit
    with BasicSessionMaterials[JwtClaims]
