package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.service.{AccountServiceEndpoints, OAuthServiceEndpoints}
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.handlers.{MockSoftPaymentAccountDao, SoftPaymentAccountDao}
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{
  GenericPaymentEndpoints,
  MockPaymentEndpoints,
  MockSoftPaymentAccountServiceEndpoints,
  MockSoftPaymentOAuthServiceEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.{SessionEndpointsRoutes, SessionTestKit}
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.{JwtClaims, Session}

import scala.concurrent.ExecutionContext

trait PaymentEndpointsTestKit extends PaymentEndpoints with SessionEndpointsRoutes[JwtClaims] {
  self: PaymentTestKit
    with SessionTestKit[JwtClaims]
    with SchemaProvider
    with CsrfCheck
    with SessionMaterials[JwtClaims] =>

  implicit def sessionConfig: SessionConfig

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = sys =>
    new MockPaymentEndpoints with SessionMaterials[JwtClaims] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[JwtClaims]
      ): SessionManager[JwtClaims] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
        self.refreshTokenStorage
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system => super.endpoints(system) :+ sessionServiceEndpoints(system)

  override def accountEndpoints
    : ActorSystem[_] => AccountServiceEndpoints[BasicAccountSignUp, JwtClaims] =
    sys =>
      new MockSoftPaymentAccountServiceEndpoints with SessionMaterials[JwtClaims] {
        override implicit def manager(implicit
          sessionConfig: SessionConfig,
          companion: SessionDataCompanion[JwtClaims]
        ): SessionManager[JwtClaims] = self.manager
        override protected def sessionType: Session.SessionType = self.sessionType
        override def log: Logger = LoggerFactory getLogger getClass.getName
        override implicit def system: ActorSystem[_] = sys
        override lazy val ec: ExecutionContext = sys.executionContext
        override protected val manifestWrapper: ManifestW = ManifestW()
        override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
          self.refreshTokenStorage
        override implicit def companion: SessionDataCompanion[JwtClaims] = self.companion
      }

  override def oauthEndpoints: ActorSystem[_] => OAuthServiceEndpoints[JwtClaims] = sys =>
    new MockSoftPaymentOAuthServiceEndpoints with SessionMaterials[JwtClaims] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[JwtClaims]
      ): SessionManager[JwtClaims] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[JwtClaims] = self.companion
    }
}
