package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.persistence.query.AccountEventProcessorStreams.InternalAccountEvents2AccountProcessorStream
import app.softnetwork.api.server.SwaggerEndpoint
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.{PaymentHandler, SoftPayAccountTypeKey}
import app.softnetwork.payment.launch.PaymentApplication
import app.softnetwork.payment.persistence.query.{
  PaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{PaymentBehavior, SoftPayAccountBehavior}
import app.softnetwork.payment.service.{
  MangoPayPaymentServiceEndpoints,
  SoftPayAccountServiceEndpoints,
  SoftPayOAuthServiceEndpoints
}
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcOffsetProvider}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.config.Settings
import app.softnetwork.session.model.{
  SessionData,
  SessionDataCompanion,
  SessionDataDecorator,
  SessionManagers
}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import com.typesafe.config.Config
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session
import sttp.tapir.swagger.SwaggerUIOptions

import scala.concurrent.ExecutionContext

trait MangoPayApi[SD <: SessionData with SessionDataDecorator[SD]] extends PaymentApplication {
  self: SchemaProvider with CsrfCheck =>

  override def internalAccountEvents2AccountProcessorStream
    : ActorSystem[_] => InternalAccountEvents2AccountProcessorStream = sys =>
    new InternalAccountEvents2AccountProcessorStream
      with SoftPayAccountTypeKey
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override def tag: String = s"${SoftPayAccountBehavior.persistenceId}-to-internal"
      override implicit def system: ActorSystem[_] = sys
    }

  override def paymentCommandProcessorStream: ActorSystem[_] => PaymentCommandProcessorStream =
    sys =>
      new PaymentCommandProcessorStream
        with PaymentHandler
        with JdbcJournalProvider
        with JdbcOffsetProvider {
        override def config: Config = MangoPayApi.this.config
        override implicit def system: ActorSystem[_] = sys
      }

  override def scheduler2PaymentProcessorStream
    : ActorSystem[_] => Scheduler2PaymentProcessorStream = sys =>
    new Scheduler2PaymentProcessorStream
      with PaymentHandler
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override val tag: String = SchedulerSettings.tag(PaymentBehavior.persistenceId)
      override implicit def system: ActorSystem[_] = sys
    }

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  implicit def companion: SessionDataCompanion[SD]

  protected def manager: SessionManager[SD] = SessionManagers.basic

  protected def refreshTokenStorage: ActorSystem[_] => RefreshTokenStorage[SD]

  override protected def sessionType: Session.SessionType =
    Settings.Session.SessionContinuityAndTransport

  def paymentSwagger: ActorSystem[_] => SwaggerEndpoint = sys =>
    new MangoPayPaymentServiceEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
      override val applicationVersion: String = systemVersion()
      override val swaggerUIOptions: SwaggerUIOptions =
        SwaggerUIOptions.default.pathPrefix(List("swagger", PaymentSettings.PaymentPath))
    }

  def accountSwagger: ActorSystem[_] => SwaggerEndpoint = sys =>
    new SoftPayAccountServiceEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = sys
      override implicit lazy val ec: ExecutionContext = sys.executionContext
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected val manifestWrapper: ManifestW = ManifestW()
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
      override val applicationVersion: String = self.systemVersion()
      override val swaggerUIOptions: SwaggerUIOptions =
        SwaggerUIOptions.default.pathPrefix(List("swagger", AccountSettings.Path))
    }

  def oauthSwagger: ActorSystem[_] => SwaggerEndpoint =
    sys =>
      new SoftPayOAuthServiceEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
        override implicit def system: ActorSystem[_] = sys
        override implicit lazy val ec: ExecutionContext = sys.executionContext
        override protected def sessionType: Session.SessionType = self.sessionType
        override implicit def sessionConfig: SessionConfig = self.sessionConfig
        override implicit def manager(implicit
          sessionConfig: SessionConfig,
          companion: SessionDataCompanion[SD]
        ): SessionManager[SD] = self.manager
        override def log: Logger = LoggerFactory getLogger getClass.getName
        override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
          self.refreshTokenStorage(sys)
        override implicit def companion: SessionDataCompanion[SD] = self.companion
        override val applicationVersion: String = self.systemVersion()
        override val swaggerUIOptions: SwaggerUIOptions =
          SwaggerUIOptions.default.pathPrefix(List("swagger", AccountSettings.OAuthPath))
      }
}
