package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.OneOffCookieSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithOneOffCookieSessionSpec
    extends PaymentServiceSpec
    with OneOffCookieSessionServiceTestKit[JwtClaims]
    with PaymentRoutesTestKit
    with BasicSessionMaterials[JwtClaims]
