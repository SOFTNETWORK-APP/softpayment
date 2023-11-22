package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials

trait MockSoftPaymentAccountServiceEndpoints
    extends SoftPaymentAccountServiceEndpoints
    with MockSoftPaymentAccountTypeKey { _: SessionMaterials => }
