package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import app.softnetwork.api.server.GrpcService
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcOffsetProvider}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.EventProcessorStream
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.api.{SchedulerApi, SchedulerServiceApiHandler}
import app.softnetwork.scheduler.handlers.SchedulerHandler
import app.softnetwork.scheduler.persistence.query.Entity2SchedulerProcessorStream
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import com.typesafe.config.Config

import scala.concurrent.Future

trait MangoPayWithSchedulerApi[SD <: SessionData with SessionDataDecorator[SD]]
    extends MangoPayApi[SD]
    with SchedulerApi { _: SchemaProvider =>

  override def entity2SchedulerProcessorStream: ActorSystem[_] => Entity2SchedulerProcessorStream =
    sys =>
      new Entity2SchedulerProcessorStream()
        with SchedulerHandler
        with JdbcJournalProvider
        with JdbcOffsetProvider {
        override def config: Config = MangoPayWithSchedulerApi.this.config
        override implicit def system: ActorSystem[_] = sys
      }

  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    schedulerEntities(sys) ++ sessionEntities(sys) ++ paymentEntities(sys)

  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    schedulerEventProcessorStreams(sys) ++
    paymentEventProcessorStreams(sys)

  /*override def initSystem: ActorSystem[_] => Unit = system => {
    initSchedulerSystem(system)
  }*/

  override def grpcServices: ActorSystem[_] => Seq[GrpcService] = system =>
    paymentGrpcServices(system) ++ schedulerGrpcServices(system)
}
