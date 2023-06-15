package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.persistence.jdbc.schema.{JdbcSchemaProvider, JdbcSchemaTypes}
import app.softnetwork.persistence.schema.SchemaType
import app.softnetwork.session.service.SessionEndpoints
import com.softwaremill.session.CsrfCheckHeader
import org.slf4j.{Logger, LoggerFactory}

object MangoPayEndpointsPostgresLauncher
    extends MangoPayEndpointsApi
    with JdbcSchemaProvider
    with CsrfCheckHeader {

  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override def schemaType: SchemaType = JdbcSchemaTypes.Postgres

  override def sessionEndpoints: ActorSystem[_] => SessionEndpoints = system =>
    SessionEndpoints.oneOffCookie(system, checkHeaderAndForm)
}
