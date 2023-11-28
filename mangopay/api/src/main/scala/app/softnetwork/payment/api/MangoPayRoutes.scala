package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.account.model.{
  DefaultAccountDetailsView,
  DefaultAccountView,
  DefaultProfileView
}
import app.softnetwork.account.service.{AccountService, OAuthService}
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.launch.PaymentRoutes
import app.softnetwork.payment.service.{
  GenericPaymentService,
  MangoPayPaymentService,
  SoftPaymentAccountService,
  SoftPaymentOAuthService
}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.service.SessionMaterials
import com.softwaremill.session.{SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait MangoPayRoutes extends PaymentRoutes { self: MangoPayApi with SchemaProvider with CsrfCheck =>

  override def paymentService: ActorSystem[_] => GenericPaymentService = sys =>
    new MangoPayPaymentService with SessionMaterials {
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
    }

  override def accountService: ActorSystem[_] => AccountService[
    DefaultProfileView,
    DefaultAccountDetailsView,
    DefaultAccountView[DefaultProfileView, DefaultAccountDetailsView]
  ] = sys =>
    new SoftPaymentAccountService with SessionMaterials {
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
//      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
      override protected val manifestWrapper: ManifestW = ManifestW()
    }

  override def oauthService: ActorSystem[_] => OAuthService = sys =>
    new SoftPaymentOAuthService with SessionMaterials {
      override implicit def manager(implicit
        sessionConfig: SessionConfig
      ): SessionManager[Session] = self.manager
      override protected def sessionType: Session.SessionType = self.sessionType
      override def log: Logger = LoggerFactory getLogger getClass.getName
//      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override implicit def system: ActorSystem[_] = sys
      override lazy val ec: ExecutionContext = sys.executionContext
    }

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] = system =>
    super.apiRoutes(system) :+ paymentSwagger(system)
}
