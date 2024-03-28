package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{GenericPaymentEndpoints, MockPaymentEndpoints}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.{SessionEndpointsRoutes, SessionTestKit}
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait PaymentEndpointsTestKit[SD <: SessionData with SessionDataDecorator[SD]]
    extends PaymentEndpoints[SD]
    with SessionEndpointsRoutes[SD] {
  self: PaymentTestKit
    with SessionTestKit[SD]
    with SchemaProvider
    with CsrfCheck
    with SessionMaterials[SD] =>

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints[SD] = sys =>
    new MockPaymentEndpoints[SD] with SessionMaterials[SD] {
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
        self.refreshTokenStorage
      override implicit def companion: SessionDataCompanion[SD] = self.companion
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system =>
      List(
        sessionServiceEndpoints(system),
        paymentEndpoints(system)
      )

}
