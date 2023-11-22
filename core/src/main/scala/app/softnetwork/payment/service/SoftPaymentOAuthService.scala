package app.softnetwork.payment.service

import akka.http.scaladsl.server.Route
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.service.OAuthService
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials

trait SoftPaymentOAuthService extends OAuthService with SoftPaymentAccountTypeKey {
  _: SessionMaterials =>

  override val route: Route = {
    pathPrefix(AccountSettings.OAuthPath) {
      concat(authorize :: (signin ++ backup).toList: _*)
    }
  }

}
