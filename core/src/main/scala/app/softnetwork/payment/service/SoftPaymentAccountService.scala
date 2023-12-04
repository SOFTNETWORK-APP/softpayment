package app.softnetwork.payment.service

import akka.http.scaladsl.server.Route
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.service.BasicAccountService
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.config.Settings
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.SessionConfig
import org.json4s.Formats
import org.softnetwork.session.model.JwtClaims

trait SoftPaymentAccountService
    extends BasicAccountService[JwtClaims]
    with SoftPaymentAccountTypeKey {
  _: SessionMaterials[JwtClaims] =>

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  override implicit lazy val formats: Formats = paymentFormats

  override val route: Route = {
    pathPrefix(AccountSettings.Path) {
      signUp ~
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
