package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message
import app.softnetwork.account.service.{AccountServiceEndpoints, OAuthServiceEndpoints}
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{
  MangoPayPaymentServiceEndpoints,
  PaymentServiceEndpoints,
  SoftPaymentAccountServiceEndpoints,
  SoftPaymentOAuthServiceEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait MangoPayEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentEndpoints[SD] {
  self: MangoPayApi[SD] with SchemaProvider with CsrfCheck =>

  override def paymentEndpoints: ActorSystem[_] => PaymentServiceEndpoints[SD] = sys =>
    new MangoPayPaymentServiceEndpoints[SD] with SessionMaterials[SD] {
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }

  override def accountEndpoints
    : ActorSystem[_] => AccountServiceEndpoints[message.BasicAccountSignUp, SD] = sys =>
    new SoftPaymentAccountServiceEndpoints[SD] with SessionMaterials[SD] {
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override protected val manifestWrapper: ManifestW = ManifestW()
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }

  override def oauthEndpoints: ActorSystem[_] => OAuthServiceEndpoints[SD] = sys =>
    new SoftPaymentOAuthServiceEndpoints[SD] with SessionMaterials[SD] {
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] = system =>
    super.endpoints(system) :+ paymentSwagger(system) :+ accountSwagger(system) :+ oauthSwagger(
      system
    )
}
