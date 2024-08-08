package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig._
import app.softnetwork.payment.handlers.{MockPaymentHandler, MockSoftPayAccountDao, SoftPayAccountDao}
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider.ProviderType
import app.softnetwork.payment.service.{MockPaymentService, PaymentService, StripeHooksDirectives}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.scalatest.{OneOffCookieSessionServiceTestKit, OneOffHeaderSessionServiceTestKit, RefreshableCookieSessionServiceTestKit, RefreshableHeaderSessionServiceTestKit, SessionTestKit}
import app.softnetwork.session.service.{JwtClaimsSessionMaterials, SessionMaterials}
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.json4s.Formats
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.{JwtClaims, Session}

import scala.concurrent.ExecutionContext

trait StripePaymentRoutesTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentRoutesTestKit[SD] {
  self: PaymentTestKit with SessionTestKit[SD] with SchemaProvider with SessionMaterials[SD] =>

  override def paymentService: ActorSystem[_] => PaymentService[SD] = sys =>
    new MockPaymentService[SD] with SessionMaterials[SD] {
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
      override lazy val hooks: Route = pathPrefix(hooksRoute) {
        pathPrefix(ProviderType.STRIPE.name.toLowerCase) {
          new StripeHooksDirectives with MockPaymentHandler {
            override def log: Logger = org.slf4j.LoggerFactory.getLogger(getClass)
            override implicit def formats: Formats = self.formats
            override implicit def system: ActorSystem[_] = sys
          }.hooks
        }
      }
    }

}

trait StripePaymentRoutesWithOneOffCookieSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with OneOffCookieSessionServiceTestKit[JwtClaims]
    with StripePaymentRoutesTestKit[JwtClaims]
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait StripePaymentRoutesWithOneOffHeaderSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with OneOffHeaderSessionServiceTestKit[JwtClaims]
    with StripePaymentRoutesTestKit[JwtClaims]
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait StripePaymentRoutesWithRefreshableCookieSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with RefreshableCookieSessionServiceTestKit[JwtClaims]
    with StripePaymentRoutesTestKit[JwtClaims]
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}

trait StripePaymentRoutesWithRefreshableHeaderSessionSpecTestKit
    extends AnyWordSpecLike
    with PaymentRouteTestKit[JwtClaims]
    with RefreshableHeaderSessionServiceTestKit[JwtClaims]
    with StripePaymentRoutesTestKit[JwtClaims]
    with JwtClaimsSessionMaterials {
  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims
}
