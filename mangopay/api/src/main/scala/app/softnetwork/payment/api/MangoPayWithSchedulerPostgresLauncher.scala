package app.softnetwork.payment.api

import akka.actor
import akka.actor.typed.ActorSystem
import app.softnetwork.persistence.jdbc.schema.{JdbcSchema, JdbcSchemaTypes}
import app.softnetwork.persistence.schema.{Schema, SchemaProvider, SchemaType}
import app.softnetwork.persistence.typed._
import com.typesafe.config.Config
import org.slf4j.{Logger, LoggerFactory}

object MangoPayWithSchedulerPostgresLauncher extends MangoPayWithSchedulerApi with SchemaProvider {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName

  override def schema: ActorSystem[_] => Schema = sys =>
    new JdbcSchema {
      override def schemaType: SchemaType = JdbcSchemaTypes.Postgres
      override implicit def classicSystem: actor.ActorSystem = sys
      override def config: Config = MangoPayWithSchedulerPostgresLauncher.this.config
    }
}
