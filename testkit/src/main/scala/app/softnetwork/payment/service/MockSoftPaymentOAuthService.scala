package app.softnetwork.payment.service

import app.softnetwork.account.spi.OAuth2Service
import app.softnetwork.payment.handlers.MockSoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials
import com.github.scribejava.core.oauth.DummyApiService
import org.softnetwork.session.model.JwtClaims

trait MockSoftPaymentOAuthService
    extends SoftPaymentOAuthService
    with MockSoftPaymentAccountTypeKey {
  _: SessionMaterials[JwtClaims] =>
  override lazy val services: Seq[OAuth2Service] =
    Seq(
      new DummyApiService()
    )
}
