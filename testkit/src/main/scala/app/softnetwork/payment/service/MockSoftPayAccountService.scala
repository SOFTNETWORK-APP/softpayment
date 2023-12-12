package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPayAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials

trait MockSoftPayAccountService[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayAccountService[SD]
    with MockSoftPayAccountTypeKey {
  _: SessionMaterials[SD] =>

}
