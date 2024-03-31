package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.service.{AccountServiceEndpoints, OAuthServiceEndpoints}
import app.softnetwork.payment.handlers.{MockSoftPayAccountDao, SoftPayAccountDao}
import app.softnetwork.payment.launch.SoftPayEndpoints
import app.softnetwork.payment.service.{
  MockSoftPayAccountServiceEndpoints,
  MockSoftPayOAuthServiceEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.SessionTestKit
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait SoftPayEndpointsTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayEndpoints[SD]
    with PaymentEndpointsTestKit[SD] {
  self: SoftPayTestKit
    with SessionTestKit[SD]
    with SchemaProvider
    with CsrfCheck
    with SessionMaterials[SD] =>

  override def accountEndpoints: ActorSystem[_] => AccountServiceEndpoints[BasicAccountSignUp, SD] =
    sys =>
      new MockSoftPayAccountServiceEndpoints[SD] with SessionMaterials[SD] {
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
    new MockSoftPayOAuthServiceEndpoints[SD] with SessionMaterials[SD] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPayAccountDao: SoftPayAccountDao = MockSoftPayAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }
}
