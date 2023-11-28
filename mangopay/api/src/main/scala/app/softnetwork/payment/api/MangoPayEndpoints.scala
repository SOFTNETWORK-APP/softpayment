package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.account.message
import app.softnetwork.account.service.{AccountServiceEndpoints, OAuthServiceEndpoints}
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.launch.PaymentEndpoints
import app.softnetwork.payment.service.{
  GenericPaymentEndpoints,
  MangoPayPaymentEndpoints,
  SoftPaymentAccountServiceEndpoints,
  SoftPaymentOAuthServiceEndpoints
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{SessionConfig, SessionManager}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait MangoPayEndpoints extends PaymentEndpoints {
  self: MangoPayApi with SchemaProvider with CsrfCheck =>
  override def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints = sys =>
    new MangoPayPaymentEndpoints with SessionMaterials {
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
    }

  override def accountEndpoints
    : ActorSystem[_] => AccountServiceEndpoints[message.BasicAccountSignUp] = sys =>
    new SoftPaymentAccountServiceEndpoints with SessionMaterials {
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
//      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override protected val manifestWrapper: ManifestW = ManifestW()
    }

  override def oauthEndpoints: ActorSystem[_] => OAuthServiceEndpoints = sys =>
    new SoftPaymentOAuthServiceEndpoints with SessionMaterials {
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
//      override implicit def sessionConfig: SessionConfig = self.sessionConfig
    }

  override def endpoints: ActorSystem[_] => List[Endpoint] = system =>
    super.endpoints(system) :+ paymentSwagger(system)
}
