package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.persistence.jdbc.schema.{JdbcSchemaProvider, JdbcSchemaTypes}
import app.softnetwork.persistence.schema.SchemaType
import app.softnetwork.session.CsrfCheckHeader
import app.softnetwork.session.handlers.JwtClaimsRefreshTokenDao
import app.softnetwork.session.model.{SessionDataCompanion, SessionManagers}
import com.softwaremill.session.{RefreshTokenStorage, SessionManager}
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.JwtClaims

object SoftPayRoutesPostgresLauncher
    extends SoftPayRoutesApi[JwtClaims]
    with JdbcSchemaProvider
    with CsrfCheckHeader { self =>
  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override def schemaType: SchemaType = JdbcSchemaTypes.Postgres

  override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

  override protected def refreshTokenStorage: ActorSystem[_] => RefreshTokenStorage[JwtClaims] =
    sys => JwtClaimsRefreshTokenDao(sys)

  override protected def manager: SessionManager[JwtClaims] = SessionManagers.jwt

}
