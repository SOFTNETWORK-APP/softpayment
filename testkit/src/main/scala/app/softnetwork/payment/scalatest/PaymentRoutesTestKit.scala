package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{GenericPaymentService, MockPaymentService}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.{SessionServiceRoutes, SessionTestKit}
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait PaymentRoutesTestKit extends PaymentRoutes with SessionServiceRoutes {
  self: PaymentTestKit with SessionTestKit with SchemaProvider with SessionMaterials =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = sys =>
    new MockPaymentService with SessionMaterials {
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
    }

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system =>
      List(
        sessionServiceRoute(system),
        paymentService(system)
      )

}
