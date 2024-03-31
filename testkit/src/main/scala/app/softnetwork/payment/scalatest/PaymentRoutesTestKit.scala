package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.handlers.{MockSoftPayAccountDao, SoftPayAccountDao}
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{MockPaymentService, PaymentService}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.scalatest.{SessionServiceRoutes, SessionTestKit}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait PaymentRoutesTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentRoutes[SD]
    with SessionServiceRoutes[SD] {
  self: PaymentTestKit with SessionTestKit[SD] with SchemaProvider with SessionMaterials[SD] =>

  override def paymentService: ActorSystem[_] => PaymentService[SD] = sys =>
    new MockPaymentService[SD] with SessionMaterials[SD] {
      override implicit def manager(implicit
        sessionConfig: SessionConfig,
        companion: SessionDataCompanion[SD]
      ): SessionManager[SD] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPayAccountDao: SoftPayAccountDao = MockSoftPayAccountDao
      override implicit def refreshTokenStorage: RefreshTokenStorage[SD] =
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }

  implicit def sessionConfig: SessionConfig

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system => super.apiRoutes(system) :+ sessionServiceRoute(system)

}
