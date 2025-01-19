package app.softnetwork.payment.service

import akka.http.scaladsl.server.Route
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.payment.handlers.MockSoftPayAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials

trait MockSoftPayAccountService[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayAccountService[SD]
    with MockSoftPayAccountTypeKey {
  _: SessionMaterials[SD] =>

  override val route: Route = {
    pathPrefix(AccountSettings.Path) {
      anonymous ~ // for testing purposes
      signUp ~
      principal ~ // for testing purposes
      basic ~ // for testing purposes
      login ~
      activate ~
      logout ~
      verificationCode ~
      resetPasswordToken ~
      resetPassword ~
      unsubscribe ~
      device ~
      password
    }
  }
}
