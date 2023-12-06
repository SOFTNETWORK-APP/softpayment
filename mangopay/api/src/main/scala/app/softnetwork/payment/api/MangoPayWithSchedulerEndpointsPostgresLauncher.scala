package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.SoftPaymentAccountDao
import app.softnetwork.payment.service.{MangoPayPaymentServiceEndpoints, PaymentServiceEndpoints}
import app.softnetwork.persistence.jdbc.schema.{JdbcSchemaProvider, JdbcSchemaTypes}
import app.softnetwork.persistence.schema.SchemaType
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.handlers.JwtClaimsRefreshTokenDao
import app.softnetwork.session.model.{SessionDataCompanion, SessionManagers}
import app.softnetwork.session.service.JwtClaimsSessionMaterials
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.{ApiKey, JwtClaims, Session}

import scala.concurrent.{ExecutionContext, Future}

object MangoPayWithSchedulerEndpointsPostgresLauncher
    extends MangoPayWithSchedulerEndpointsApi[JwtClaims]
    with JdbcSchemaProvider
    with CsrfCheckHeader { self =>

  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override def schemaType: SchemaType = JdbcSchemaTypes.Postgres

  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

  override protected def refreshTokenStorage: ActorSystem[_] => RefreshTokenStorage[JwtClaims] =
    sys => JwtClaimsRefreshTokenDao(sys)

  override protected def manager: SessionManager[JwtClaims] = SessionManagers.jwt

  override def paymentEndpoints: ActorSystem[_] => PaymentServiceEndpoints[JwtClaims] = sys =>
    new MangoPayPaymentServiceEndpoints[JwtClaims] with JwtClaimsSessionMaterials {
      override implicit def system: ActorSystem[_] = sys
      override implicit lazy val ec: ExecutionContext = sys.executionContext
      override implicit def sessionConfig: SessionConfig = self.sessionConfig
      override def log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
        self.refreshTokenStorage(sys)
      override implicit def companion: SessionDataCompanion[JwtClaims] = self.companion
      override protected def sessionType: Session.SessionType = self.sessionType
    }
}
