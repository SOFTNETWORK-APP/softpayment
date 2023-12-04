package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.handlers.AccountDao
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.account.persistence.query.AccountEventProcessorStreams.InternalAccountEvents2AccountProcessorStream
import app.softnetwork.account.persistence.typed.AccountBehavior
import app.softnetwork.api.server.SwaggerEndpoint
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.{
  MangoPayPaymentHandler,
  SoftPaymentAccountDao,
  SoftPaymentAccountTypeKey
}
import app.softnetwork.payment.launch.PaymentApplication
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.payment.persistence.query.{
  GenericPaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{
  GenericPaymentBehavior,
  MangoPayPaymentBehavior,
  SoftPaymentAccountBehavior
}
import app.softnetwork.payment.service.{
  MangoPayPaymentEndpoints,
  SoftPaymentAccountServiceEndpoints,
  SoftPaymentOAuthServiceEndpoints
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

import scala.concurrent.{ExecutionContext, Future}

trait MangoPayApi[SD <: SessionData with SessionDataDecorator[SD]] extends PaymentApplication {
  self: SchemaProvider with CsrfCheck =>

  override def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior = _ =>
    MangoPayPaymentBehavior

  override def accountBehavior
    : ActorSystem[_] => AccountBehavior[SoftPaymentAccount, BasicAccountProfile] = _ =>
    SoftPaymentAccountBehavior

  override def internalAccountEvents2AccountProcessorStream
    : ActorSystem[_] => InternalAccountEvents2AccountProcessorStream = sys =>
    new InternalAccountEvents2AccountProcessorStream
      with SoftPaymentAccountTypeKey
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override def tag: String = s"${SoftPaymentAccountBehavior.persistenceId}-to-internal"
      override implicit def system: ActorSystem[_] = sys
    }

  override def paymentCommandProcessorStream
    : ActorSystem[_] => GenericPaymentCommandProcessorStream = sys =>
    new GenericPaymentCommandProcessorStream
      with MangoPayPaymentHandler
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override implicit def system: ActorSystem[_] = sys
    }

  override def scheduler2PaymentProcessorStream
    : ActorSystem[_] => Scheduler2PaymentProcessorStream = sys =>
    new Scheduler2PaymentProcessorStream
      with MangoPayPaymentHandler
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override val tag: String = SchedulerSettings.tag(MangoPayPaymentBehavior.persistenceId)
      override implicit def system: ActorSystem[_] = sys
    }

  override def grpcServices
    : ActorSystem[_] => Seq[PartialFunction[HttpRequest, Future[HttpResponse]]] =
    system =>
      Seq(
        PaymentServiceApiHandler.partial(MangoPayServer(system))(system)
      )

  implicit def sessionConfig: SessionConfig = Settings.Session.DefaultSessionConfig

  implicit def companion: SessionDataCompanion[SD]

  protected def manager: SessionManager[SD] = SessionManagers.basic

  protected def refreshTokenStorage: ActorSystem[_] => RefreshTokenStorage[SD]

  override protected def sessionType: Session.SessionType =
    Settings.Session.SessionContinuityAndTransport

  def paymentSwagger: ActorSystem[_] => SwaggerEndpoint = sys =>
    new MangoPayPaymentEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
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
    new SoftPaymentAccountServiceEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
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
      new SoftPaymentOAuthServiceEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
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
