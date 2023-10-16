package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import app.softnetwork.api.server.SwaggerEndpoint
import app.softnetwork.payment.handlers.MangoPayPaymentHandler
import app.softnetwork.payment.launch.PaymentApplication
import app.softnetwork.payment.persistence.query.{
  GenericPaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{GenericPaymentBehavior, MangoPayPaymentBehavior}
import app.softnetwork.payment.service.MangoPayPaymentEndpoints
import app.softnetwork.persistence.jdbc.query.{JdbcJournalProvider, JdbcOffsetProvider}
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.service.SessionEndpoints
import com.typesafe.config.Config
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

trait MangoPayApi extends PaymentApplication { self: SchemaProvider with CsrfCheck =>

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

  override def grpcServices
    : ActorSystem[_] => Seq[PartialFunction[HttpRequest, Future[HttpResponse]]] =
    system =>
      Seq(
        PaymentServiceApiHandler.partial(MangoPayServer(system))(system)
      )

  def paymentSwagger: ActorSystem[_] => SwaggerEndpoint = sys =>
    new MangoPayPaymentEndpoints with SwaggerEndpoint {
      override implicit def system: ActorSystem[_] = sys
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override def sessionEndpoints: SessionEndpoints = self.sessionEndpoints(system)
      override val applicationVersion: String = systemVersion()
    }
}
