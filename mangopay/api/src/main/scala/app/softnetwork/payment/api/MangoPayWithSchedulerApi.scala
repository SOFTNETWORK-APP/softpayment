package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcSchema, JdbcSchemaProvider}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.EventProcessorStream
import app.softnetwork.scheduler.api.{SchedulerApi, SchedulerServiceApiHandler}
import app.softnetwork.scheduler.handlers.SchedulerHandler
import app.softnetwork.scheduler.persistence.query.Entity2SchedulerProcessorStream

import scala.concurrent.Future

trait MangoPayWithSchedulerApi extends MangoPayApi with SchedulerApi {

  override def entity2SchedulerProcessorStream: ActorSystem[_] => Entity2SchedulerProcessorStream =
    sys =>
      new Entity2SchedulerProcessorStream()
        with SchedulerHandler
        with JdbcJournalProvider
        with JdbcSchemaProvider {
        override lazy val schemaType: JdbcSchema.SchemaType = MangoPayWithSchedulerApi.this.schemaType
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

  override def grpcServices
    : ActorSystem[_] => Seq[PartialFunction[HttpRequest, Future[HttpResponse]]] = system =>
    Seq(
      PaymentServiceApiHandler.partial(MangoPayServer(system))(system)
    ) :+ SchedulerServiceApiHandler.partial(schedulerServer(system))(system)
}
