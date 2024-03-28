package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.PaymentRoutesTestKit
import app.softnetwork.session.handlers.SessionRefreshTokenDao
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.scalatest.RefreshableCookieSessionServiceTestKit
import app.softnetwork.session.service.BasicSessionMaterials
import com.softwaremill.session.RefreshTokenStorage
import org.softnetwork.session.model.Session

class PaymentRoutesWithRefreshableCookieSessionSpec
    extends PaymentServiceSpec[Session]
    with RefreshableCookieSessionServiceTestKit[Session]
    with PaymentRoutesTestKit[Session]
    with BasicSessionMaterials[Session] {
  override implicit def companion: SessionDataCompanion[Session] = Session

  override implicit def refreshTokenStorage: RefreshTokenStorage[Session] = SessionRefreshTokenDao(
    ts
  )
}
