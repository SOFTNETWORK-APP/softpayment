package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.SwaggerEndpoint
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.MangoPayPaymentHandler
import app.softnetwork.payment.launch.PaymentApplication
import app.softnetwork.payment.persistence.query.{
  GenericPaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{GenericPaymentBehavior, MangoPayPaymentBehavior}
import app.softnetwork.payment.service.MangoPayPaymentEndpoints
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcOffsetProvider}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.api.SessionDataApi
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import com.typesafe.config.Config
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session
import sttp.tapir.swagger.SwaggerUIOptions

import scala.concurrent.ExecutionContext

trait MangoPayApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentApplication
    with SessionDataApi[SD] {
  self: SchemaProvider with CsrfCheck =>

  override def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior = _ =>
    MangoPayPaymentBehavior

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

  override def paymentServer: ActorSystem[_] => PaymentServer = system => MangoPayServer(system)

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
}
