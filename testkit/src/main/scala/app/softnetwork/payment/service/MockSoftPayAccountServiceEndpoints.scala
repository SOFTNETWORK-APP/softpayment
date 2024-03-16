package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPayAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials

trait MockSoftPayAccountServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayAccountServiceEndpoints[SD]
    with MockSoftPayAccountTypeKey { _: SessionMaterials[SD] => }
