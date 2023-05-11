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
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcOffsetProvider}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.config.SchedulerSettings
import com.typesafe.config.Config

import scala.concurrent.Future

trait MangoPayApi extends PaymentApplication { _: SchemaProvider =>

  override def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior = _ =>
    MangoPayPaymentBehavior

  override def paymentCommandProcessorStream
    : ActorSystem[_] => GenericPaymentCommandProcessorStream = sys =>
    new GenericPaymentCommandProcessorStream
      with MangoPayPaymentHandler
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override implicit def system: ActorSystem[_] = sys
    }

  override def scheduler2PaymentProcessorStream
    : ActorSystem[_] => Scheduler2PaymentProcessorStream = sys =>
    new Scheduler2PaymentProcessorStream
      with MangoPayPaymentHandler
      with JdbcJournalProvider
      with JdbcOffsetProvider {
      override def config: Config = MangoPayApi.this.config
      override val tag: String = SchedulerSettings.tag(MangoPayPaymentBehavior.persistenceId)
      override implicit def system: ActorSystem[_] = sys
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
