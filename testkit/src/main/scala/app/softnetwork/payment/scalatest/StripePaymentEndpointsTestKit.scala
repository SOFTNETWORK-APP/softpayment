package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.{
  MockPaymentHandler,
  MockSoftPayAccountDao,
  SoftPayAccountDao
}
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider.ProviderType
import app.softnetwork.payment.service.{
  MockPaymentServiceEndpoints,
  PaymentServiceEndpoints,
  StripeHooksEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.{CsrfCheck, CsrfCheckHeader}
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.scalatest.{
  OneOffCookieSessionEndpointsTestKit,
  OneOffHeaderSessionEndpointsTestKit,
  RefreshableCookieSessionEndpointsTestKit,
  RefreshableHeaderSessionEndpointsTestKit,
  SessionTestKit
}
import app.softnetwork.session.service.{JwtClaimsSessionMaterials, SessionMaterials}
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.{JwtClaims, Session}
import sttp.tapir.server.ServerEndpoint.Full

import scala.concurrent.{ExecutionContext, Future}

trait StripePaymentEndpointsTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentEndpointsTestKit[SD] {
  self: PaymentTestKit
    with SessionTestKit[SD]
    with SchemaProvider
    with CsrfCheck
    with SessionMaterials[SD] =>

  override def paymentEndpoints: ActorSystem[_] => PaymentServiceEndpoints[SD] = sys =>
    new MockPaymentServiceEndpoints[SD] with SessionMaterials[SD] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPayAccountDao: SoftPayAccountDao = MockSoftPayAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[SD] = self.companion
      override lazy val hooks: List[Full[Unit, Unit, _, Unit, Unit, Any, Future]] = List(
        new StripeHooksEndpoints with MockPaymentHandler {
          override def log: Logger = org.slf4j.LoggerFactory.getLogger(getClass)

          override implicit def system: ActorSystem[_] = sys
        }.hooks(
          rootEndpoint
            .in(PaymentSettings.PaymentConfig.hooksRoute)
            .in(ProviderType.STRIPE.name.toLowerCase)
        )
      )
    }

}

trait StripePaymentEndpointsWithOneOffCookieSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with OneOffCookieSessionEndpointsTestKit[JwtClaims]
    with StripePaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait StripePaymentEndpointsWithOneOffHeaderSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with OneOffHeaderSessionEndpointsTestKit[JwtClaims]
    with StripePaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait StripePaymentEndpointsWithRefreshableCookieSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with RefreshableCookieSessionEndpointsTestKit[JwtClaims]
    with StripePaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait StripePaymentEndpointsWithRefreshableHeaderSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with RefreshableHeaderSessionEndpointsTestKit[JwtClaims]
    with StripePaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
