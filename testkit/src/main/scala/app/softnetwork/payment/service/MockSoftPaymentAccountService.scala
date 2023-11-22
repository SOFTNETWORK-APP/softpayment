package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.MockSoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials

trait MockSoftPaymentAccountService
    extends SoftPaymentAccountService
    with MockSoftPaymentAccountTypeKey {
  _: SessionMaterials =>

}
