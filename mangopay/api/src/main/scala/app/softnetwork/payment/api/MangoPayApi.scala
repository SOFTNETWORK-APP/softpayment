package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.handlers.MangoPayPaymentHandler
import app.softnetwork.payment.launch.PaymentApplication
import app.softnetwork.payment.persistence.query.{
  GenericPaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{GenericPaymentBehavior, MangoPayPaymentBehavior}
import app.softnetwork.payment.service.{GenericPaymentService, MangoPayPaymentService}
import app.softnetwork.persistence.jdbc.query.JdbcSchema.SchemaType
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcSchema, JdbcSchemaProvider}
import app.softnetwork.scheduler.config.SchedulerSettings

import scala.concurrent.Future

trait MangoPayApi extends PaymentApplication with JdbcSchemaProvider {

  def jdbcSchemaType: SchemaType = this.schemaType

  override def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior = _ =>
    MangoPayPaymentBehavior

  override def paymentCommandProcessorStream
    : ActorSystem[_] => GenericPaymentCommandProcessorStream = sys =>
    new GenericPaymentCommandProcessorStream
      with MangoPayPaymentHandler
      with JdbcJournalProvider
      with JdbcSchemaProvider {
      override implicit def system: ActorSystem[_] = sys
      override def schemaType: JdbcSchema.SchemaType = jdbcSchemaType
    }

  override def scheduler2PaymentProcessorStream
    : ActorSystem[_] => Scheduler2PaymentProcessorStream = sys =>
    new Scheduler2PaymentProcessorStream
      with MangoPayPaymentHandler
      with JdbcJournalProvider
      with JdbcSchemaProvider {
      override val tag: String = SchedulerSettings.tag(MangoPayPaymentBehavior.persistenceId)
      override implicit def system: ActorSystem[_] = sys
      override def schemaType: JdbcSchema.SchemaType = jdbcSchemaType
    }

  override def paymentService: ActorSystem[_] => GenericPaymentService = sys =>
    MangoPayPaymentService(sys)

  override def grpcServices
    : ActorSystem[_] => Seq[PartialFunction[HttpRequest, Future[HttpResponse]]] =
    system =>
      Seq(
        PaymentServiceApiHandler.partial(MangoPayServer(system))(system)
      )

  override def apiRoutes(system: ActorSystem[_]): Route =
    super.apiRoutes(system) ~ BasicServiceRoute(system).route

}
