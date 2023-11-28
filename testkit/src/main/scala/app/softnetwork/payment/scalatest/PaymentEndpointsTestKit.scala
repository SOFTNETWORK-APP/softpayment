package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.service.{AccountServiceEndpoints, OAuthServiceEndpoints}
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.handlers.{MockSoftPaymentAccountDao, SoftPaymentAccountDao}
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{
  GenericPaymentEndpoints,
  MockPaymentEndpoints,
  MockSoftPaymentAccountServiceEndpoints,
  MockSoftPaymentOAuthServiceEndpoints,
  SoftPaymentAccountServiceEndpoints,
  SoftPaymentOAuthServiceEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.scalatest.{SessionEndpointsRoutes, SessionTestKit}
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait PaymentEndpointsTestKit extends PaymentEndpoints with SessionEndpointsRoutes {
  self: PaymentTestKit
    with SessionTestKit
    with SchemaProvider
    with CsrfCheck
    with SessionMaterials =>

  implicit def sessionConfig: SessionConfig

  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = sys =>
    new MockPaymentEndpoints with SessionMaterials {
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system => super.endpoints(system) :+ sessionServiceEndpoints(system)

  override def accountEndpoints: ActorSystem[_] => AccountServiceEndpoints[BasicAccountSignUp] =
    sys =>
      new MockSoftPaymentAccountServiceEndpoints with SessionMaterials {
        override implicit def manager(implicit
          sessionConfig: SessionConfig
        ): SessionManager[Session] = self.manager
        override protected def sessionType: Session.SessionType = self.sessionType
        override def log: Logger = LoggerFactory getLogger getClass.getName
//        override implicit def sessionConfig: SessionConfig = self.sessionConfig
        override implicit def system: ActorSystem[_] = sys
        override lazy val ec: ExecutionContext = sys.executionContext
        override protected val manifestWrapper: ManifestW = ManifestW()
      }

  override def oauthEndpoints: ActorSystem[_] => OAuthServiceEndpoints = sys =>
    new MockSoftPaymentOAuthServiceEndpoints with SessionMaterials {
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
//      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
    }
}
