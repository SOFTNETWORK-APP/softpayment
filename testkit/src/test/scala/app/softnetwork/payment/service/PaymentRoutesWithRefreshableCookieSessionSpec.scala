package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.scalatest.RefreshableCookieSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials
import org.softnetwork.session.model.JwtClaims

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec
    with RefreshableCookieSessionServiceTestKit[JwtClaims]
    with PaymentRoutesTestKit
    with BasicSessionMaterials[JwtClaims]
