package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.persistence.audit.AuditLog
import ch.qos.logback.classic.{Logger => LogbackLogger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.stripe.Stripe
import com.stripe.model.Event
import com.stripe.net.ApiResource
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.{Logger, LoggerFactory}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.reflect.ClassTag

/** Story 13.7 — handler-level proof (no Stripe network) that the webhook path stamps the Stripe
  * event id as the correlation id onto the dispatched payment command, the durable hop that rides
  * the command → the persisted payment event → the licensing pod. `run` is overridden to capture
  * the command instead of dispatching it, so no cluster/entity is needed.
  */
class StripeWebhookCorrelationSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val testSystem: ActorSystem[Nothing] =
    ActorSystem(Behaviors.empty, "stripe-webhook-correlation")

  override def afterAll(): Unit = testSystem.terminate()

  private val captured = new AtomicReference[List[PaymentCommandWithKey]](Nil)

  private val handler =
    new StripeEventHandler with BasicPaymentService with MockPaymentHandler {
      override def log: Logger = LoggerFactory.getLogger(getClass)
      override implicit def system: ActorSystem[_] = testSystem
      // capture instead of dispatching to a sharded entity
      override def run(command: PaymentCommandWithKey)(implicit
        tTag: ClassTag[PaymentCommand]
      ): Future[PaymentResult] = {
        captured.updateAndGet(command :: _)
        Future.successful(PaymentAccountNotFound) // benign; the handler only logs on the result
      }
    }

  private def stripeEvent(eventId: String, eventType: String, dataObject: String): Event = {
    val json =
      s"""{"id":"$eventId","object":"event","api_version":"${Stripe.API_VERSION}",""" +
      s""""type":"$eventType","data":{"object":$dataObject}}"""
    ApiResource.GSON.fromJson(json, classOf[Event])
  }

  "StripeEventHandler" should {
    "resolve the correlation id from the Stripe object metadata and emit webhook_received (Story 13.7)" in {
      captured.set(Nil)
      val auditLogger = LoggerFactory.getLogger(AuditLog.LoggerName).asInstanceOf[LogbackLogger]
      val appender = new ListAppender[ILoggingEvent]()
      appender.start()
      auditLogger.addAppender(appender)
      try {
        val event = stripeEvent(
          "evt_meta_1",
          "customer.subscription.deleted",
          """{"id":"sub_meta","object":"subscription","customer":"cus_test",""" +
          """"metadata":{"correlation_id":"cid-from-meta"}}"""
        )
        handler.handleStripeEvent(event)
        // the metadata correlation_id wins over the event id / subscription id
        val cmd = captured.get().headOption
        cmd shouldBe defined
        cmd.get shouldBe a[UpdateRecurringCardPaymentRegistration]
        cmd.get.correlationId shouldBe Some("cid-from-meta")
        // the inbound webhook is audited, keyed by the Stripe event id
        val received =
          appender.list.toArray.toList.collect { case e: ILoggingEvent => e }.find { e =>
            val fields = e.getArgumentArray.map(_.toString).toSet
            fields.contains("event_type=webhook_received") && fields.contains(
              "correlation_id=evt_meta_1"
            )
          }
        assert(received.isDefined, "expected a webhook_received audit line carrying the event id")
        received.get.getArgumentArray.map(_.toString) should contain("service=payment")
      } finally {
        auditLogger.detachAppender(appender)
        appender.stop()
      }
    }

    "fall back to the subscription id when no correlation id is in metadata (Story 13.7)" in {
      captured.set(Nil)
      val event = stripeEvent(
        "evt_fallback_2",
        "customer.subscription.deleted",
        """{"id":"sub_fallback","object":"subscription","customer":"cus_test"}"""
      )
      handler.handleStripeEvent(event)
      val cmd = captured.get().headOption
      cmd shouldBe defined
      cmd.get shouldBe a[UpdateRecurringCardPaymentRegistration]
      // no metadata cid → the subscription id is the durable correlation key (cf RecurringPaymentCommandHandler)
      cmd.get.correlationId shouldBe Some("sub_fallback")
    }
  }
}
