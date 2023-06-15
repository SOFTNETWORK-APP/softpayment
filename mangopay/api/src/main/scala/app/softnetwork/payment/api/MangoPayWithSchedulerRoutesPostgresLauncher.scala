package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.persistence.jdbc.schema.{JdbcSchemaProvider, JdbcSchemaTypes}
import app.softnetwork.persistence.schema.SchemaType
import app.softnetwork.session.service.SessionService
import org.slf4j.{Logger, LoggerFactory}

object MangoPayWithSchedulerRoutesPostgresLauncher
    extends MangoPayWithSchedulerRoutesApi
    with JdbcSchemaProvider {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override def schemaType: SchemaType = JdbcSchemaTypes.Postgres

  override def sessionService: ActorSystem[_] => SessionService = system =>
    SessionService.oneOffCookie(system)

}
