package app.softnetwork.payment.service

import akka.http.scaladsl.server.Route
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.service.BasicAccountService
import app.softnetwork.payment.config.PaymentSettings.ClientSessionConfig
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.SessionConfig
import org.json4s.Formats

trait SoftPaymentAccountService extends BasicAccountService with SoftPaymentAccountTypeKey {
  _: SessionMaterials =>

  implicit def sessionConfig: SessionConfig = ClientSessionConfig

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
