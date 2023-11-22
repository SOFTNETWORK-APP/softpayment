package app.softnetwork.payment.service

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.Unmarshaller
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.message.SignUp
import app.softnetwork.account.service.BasicAccountService
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.payment.message.AccountMessages.SoftPaymentSignup
import app.softnetwork.session.service.SessionMaterials

trait SoftPaymentAccountService extends BasicAccountService with SoftPaymentAccountTypeKey {
  _: SessionMaterials =>

  override val route: Route = {
    pathPrefix(AccountSettings.Path) {
      signUp ~
      basic ~
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
