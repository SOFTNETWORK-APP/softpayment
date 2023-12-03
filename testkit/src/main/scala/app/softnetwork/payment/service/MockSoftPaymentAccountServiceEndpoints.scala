package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials
import org.softnetwork.session.model.JwtClaims

trait MockSoftPaymentAccountServiceEndpoints
    extends SoftPaymentAccountServiceEndpoints
    with MockSoftPaymentAccountTypeKey { _: SessionMaterials[JwtClaims] => }
