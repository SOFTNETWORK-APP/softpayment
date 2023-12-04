package app.softnetwork.payment.service

import app.softnetwork.account.spi.OAuth2Service
import app.softnetwork.payment.handlers.MockSoftPaymentAccountTypeKey
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.github.scribejava.core.oauth.DummyApiService

trait MockSoftPaymentOAuthServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPaymentOAuthServiceEndpoints[SD]
    with MockSoftPaymentAccountTypeKey { _: SessionMaterials[SD] =>

  override lazy val services: Seq[OAuth2Service] =
    Seq(
      new DummyApiService()
    )
}
