package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.model.{
  DefaultAccountDetailsView,
  DefaultAccountView,
  DefaultProfileView
}
import app.softnetwork.account.service.{AccountService, OAuthService}
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.handlers.{MockSoftPaymentAccountDao, SoftPaymentAccountDao}
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{
  GenericPaymentService,
  MockPaymentService,
  MockSoftPaymentAccountService,
  MockSoftPaymentOAuthService
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.scalatest.{SessionServiceRoutes, SessionTestKit}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.{JwtClaims, Session}

import scala.concurrent.ExecutionContext

trait PaymentRoutesTestKit extends PaymentRoutes with SessionServiceRoutes[JwtClaims] {
  self: PaymentTestKit
    with SessionTestKit[JwtClaims]
    with SchemaProvider
    with SessionMaterials[JwtClaims] =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = sys =>
    new MockPaymentService with SessionMaterials[JwtClaims] {
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

  implicit def sessionConfig: SessionConfig

  override def accountService: ActorSystem[_] => AccountService[
    DefaultProfileView,
    DefaultAccountDetailsView,
    DefaultAccountView[DefaultProfileView, DefaultAccountDetailsView],
    JwtClaims
  ] = sys =>
    new MockSoftPaymentAccountService with SessionMaterials[JwtClaims] {
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

  override def oauthService: ActorSystem[_] => OAuthService[JwtClaims] = sys =>
    new MockSoftPaymentOAuthService with SessionMaterials[JwtClaims] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[JwtClaims]
      ): SessionManager[JwtClaims] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
//      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[JwtClaims] = self.companion
    }

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system => super.apiRoutes(system) :+ sessionServiceRoute(system)

}
