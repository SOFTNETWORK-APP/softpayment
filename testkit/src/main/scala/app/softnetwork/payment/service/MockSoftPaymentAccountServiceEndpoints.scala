package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPaymentAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials

trait MockSoftPaymentAccountServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPaymentAccountServiceEndpoints[SD]
    with MockSoftPaymentAccountTypeKey { _: SessionMaterials[SD] => }
