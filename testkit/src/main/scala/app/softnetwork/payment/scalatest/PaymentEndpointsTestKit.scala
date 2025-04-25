package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.handlers.{MockSoftPayAccountDao, SoftPayAccountDao}
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.{MockPaymentServiceEndpoints, PaymentServiceEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.{CsrfCheck, CsrfCheckHeader}
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.scalatest.{
  OneOffCookieSessionEndpointsTestKit,
  OneOffHeaderSessionEndpointsTestKit,
  RefreshableCookieSessionEndpointsTestKit,
  RefreshableHeaderSessionEndpointsTestKit,
  SessionEndpointsRoutes,
  SessionTestKit
}
import app.softnetwork.session.service.{JwtClaimsSessionMaterials, SessionMaterials}
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.json4s.Formats
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.{JwtClaims, Session}

import scala.concurrent.ExecutionContext

trait PaymentEndpointsTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentEndpoints[SD]
    with SessionEndpointsRoutes[SD] {
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
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system => super.endpoints(system) :+ sessionServiceEndpoints(system)

}

trait PaymentEndpointsWithOneOffCookieSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with OneOffCookieSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def formats: Formats = paymentFormats

  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait PaymentEndpointsWithOneOffHeaderSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with OneOffHeaderSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def formats: Formats = paymentFormats

  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait PaymentEndpointsWithRefreshableCookieSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with RefreshableCookieSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def formats: Formats = paymentFormats

  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait PaymentEndpointsWithRefreshableHeaderSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with RefreshableHeaderSessionEndpointsTestKit[JwtClaims]
    with PaymentEndpointsTestKit[JwtClaims]
    with CsrfCheckHeader
    with JwtClaimsSessionMaterials {
  override implicit def formats: Formats = paymentFormats

  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
