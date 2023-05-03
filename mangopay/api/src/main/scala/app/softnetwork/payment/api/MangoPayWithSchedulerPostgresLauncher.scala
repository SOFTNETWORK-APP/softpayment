package app.softnetwork.payment.api

import akka.actor.{typed, ActorSystem}
import app.softnetwork.persistence.jdbc.schema.PostgresSchemaProvider
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.persistence.typed._
import com.typesafe.config.Config
import org.slf4j.{Logger, LoggerFactory}

object MangoPayWithSchedulerPostgresLauncher extends MangoPayWithSchedulerApi {
  lazy val log: Logger = LoggerFactory getLogger getClass.getName
  override def schemaProvider: typed.ActorSystem[_] => SchemaProvider = sys =>
    new PostgresSchemaProvider {
      override implicit def classicSystem: ActorSystem = sys
      override def config: Config = MangoPayWithSchedulerPostgresLauncher.this.config
    }
}
