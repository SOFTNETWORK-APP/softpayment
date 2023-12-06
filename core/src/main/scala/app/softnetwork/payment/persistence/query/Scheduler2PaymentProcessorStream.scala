package app.softnetwork.payment.persistence.query

import akka.actor.typed.eventstream.EventStream.Publish
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  PaymentCommand,
  PaymentResult,
  Schedule4PaymentTriggered,
  TriggerSchedule4Payment
}
import app.softnetwork.persistence.query.{JournalProvider, OffsetProvider}
import app.softnetwork.scheduler.persistence.query.Scheduler2EntityProcessorStream
import app.softnetwork.scheduler.model.Schedule

import scala.concurrent.Future

trait Scheduler2PaymentProcessorStream
    extends Scheduler2EntityProcessorStream[PaymentCommand, PaymentResult] {
  _: PaymentHandler with JournalProvider with OffsetProvider =>

  /** @param schedule
    *   - the schedule to trigger
    * @return
    *   true if the schedule has been successfully triggered, false otherwise
    */
  override protected def triggerSchedule(schedule: Schedule): Future[Boolean] = {
    !?(TriggerSchedule4Payment(schedule)) map {
      case result: Schedule4PaymentTriggered =>
        if (forTests) {
          system.eventStream.tell(Publish(result))
        }
        true
      case other =>
        if (forTests) {
          system.eventStream.tell(Publish(other))
        }
        false
    }
  }
}
