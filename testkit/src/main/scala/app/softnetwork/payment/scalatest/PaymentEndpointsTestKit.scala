package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.service.{AccountServiceEndpoints, OAuthServiceEndpoints}
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.handlers.{MockSoftPaymentAccountDao, SoftPaymentAccountDao}
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{
  MockPaymentServiceEndpoints,
  MockSoftPaymentAccountServiceEndpoints,
  MockSoftPaymentOAuthServiceEndpoints,
  PaymentServiceEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.{SessionEndpointsRoutes, SessionTestKit}
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
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

  implicit def sessionConfig: SessionConfig

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
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system => super.endpoints(system) :+ sessionServiceEndpoints(system)

  override def accountEndpoints: ActorSystem[_] => AccountServiceEndpoints[BasicAccountSignUp, SD] =
    sys =>
      new MockSoftPaymentAccountServiceEndpoints[SD] with SessionMaterials[SD] {
        override implicit def manager(implicit
          sessionConfig: SessionConfig,
          companion: SessionDataCompanion[SD]
        ): SessionManager[SD] = self.manager
        override protected def sessionType: Session.SessionType = self.sessionType
        override def log: Logger = LoggerFactory getLogger getClass.getName
        override implicit def system: ActorSystem[_] = sys
        override lazy val ec: ExecutionContext = sys.executionContext
        override protected val manifestWrapper: ManifestW = ManifestW()
        override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
          self.refreshTokenStorage
        override implicit def companion: SessionDataCompanion[SD] = self.companion
      }

  override def oauthEndpoints: ActorSystem[_] => OAuthServiceEndpoints[SD] = sys =>
    new MockSoftPaymentOAuthServiceEndpoints[SD] with SessionMaterials[SD] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }
}
