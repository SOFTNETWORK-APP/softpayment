package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.{ApiRoute, SwaggerEndpoint}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.launch.SchedulerRoutes
import app.softnetwork.scheduler.service.{SchedulerService, SchedulerServiceEndpoints}
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session
import sttp.tapir.swagger.SwaggerUIOptions

import scala.concurrent.ExecutionContext

trait SoftPayWithSchedulerRoutesApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends SoftPayWithSchedulerApi[SD]
    with SchedulerRoutes[SD]
    with SoftPayRoutesApi[SD] { self: SchemaProvider =>
  override def schedulerService: ActorSystem[_] => SchedulerService[SD] = sys =>
    new SchedulerService[SD] with SessionMaterials[SD] {
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[SD] = self.companion
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
    }

  private def schedulerSwagger: ActorSystem[_] => SwaggerEndpoint =
    sys =>
      new SchedulerServiceEndpoints[SD] with SwaggerEndpoint with SessionMaterials[SD] {
        override implicit def system: ActorSystem[_] = sys
        override lazy val ec: ExecutionContext = sys.executionContext
        lazy val log: Logger = LoggerFactory getLogger getClass.getName
        override protected def sessionType: Session.SessionType = self.sessionType
        override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
          self.refreshTokenStorage(sys)
        override implicit def companion: SessionDataCompanion[SD] = self.companion
        override implicit def manager(implicit
          sessionConfig: SessionConfig,
          companion: SessionDataCompanion[SD]
        ): SessionManager[SD] = self.manager
        override val applicationVersion: String = systemVersion()
        override val swaggerUIOptions: SwaggerUIOptions =
          SwaggerUIOptions.default.pathPrefix(List("swagger", SchedulerSettings.SchedulerPath))
      }

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system => super.apiRoutes(system) :+ schedulerService(system) :+ schedulerSwagger(system)
}
