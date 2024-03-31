package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.{Endpoint, SwaggerApiEndpoint}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.launch.SchedulerEndpoints
import app.softnetwork.scheduler.service.SchedulerServiceEndpoints
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session
import sttp.tapir.swagger.SwaggerUIOptions

import scala.concurrent.ExecutionContext

trait SoftPayWithSchedulerEndpointsApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayWithSchedulerApi[SD]
    with SchedulerEndpoints[SD]
    with SoftPayEndpointsApi[SD] { self: SchemaProvider with CsrfCheck =>
  override def schedulerEndpoints: ActorSystem[_] => SchedulerServiceEndpoints[SD] = sys =>
    new SchedulerServiceEndpoints[SD] with SwaggerApiEndpoint with SessionMaterials[SD] {
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
      override val applicationVersion: String = systemVersion()
      override val swaggerUIOptions: SwaggerUIOptions =
        SwaggerUIOptions.default.pathPrefix(List("swagger", SchedulerSettings.SchedulerPath))
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] = system =>
    super.endpoints(system) :+ schedulerEndpoints(system)
}
