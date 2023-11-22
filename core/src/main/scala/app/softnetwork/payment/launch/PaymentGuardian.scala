package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.PaymentCoreBuildInfo
import app.softnetwork.payment.persistence.data.paymentKvDao
import app.softnetwork.payment.persistence.query.{
  GenericPaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{
  GenericPaymentBehavior,
  SoftPaymentAccountBehavior
}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.EventProcessorStream
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.persistence.typed.Singleton
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.launch.SessionGuardian

trait PaymentGuardian extends SessionGuardian { _: SchemaProvider with CsrfCheck =>

  import app.softnetwork.persistence.launch.PersistenceGuardian._

  def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior

  def softPaymentAccountBehavior: ActorSystem[_] => SoftPaymentAccountBehavior

  def paymentEntities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    Seq(
      paymentAccountBehavior(sys),
      softPaymentAccountBehavior(sys)
    )

  /** initialize all entities
    */
  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    sessionEntities(sys) ++ paymentEntities(sys)

  /** initialize all singletons
    */
  override def singletons: ActorSystem[_] => Seq[Singleton[_]] = _ => Seq(paymentKvDao)

  def paymentCommandProcessorStream: ActorSystem[_] => GenericPaymentCommandProcessorStream

  def scheduler2PaymentProcessorStream: ActorSystem[_] => Scheduler2PaymentProcessorStream

  def paymentEventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    Seq(paymentCommandProcessorStream(sys)) :+ scheduler2PaymentProcessorStream(sys)

  /** initialize all event processor streams
    */
  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    paymentEventProcessorStreams(sys)

  override def systemVersion(): String =
    sys.env.getOrElse("VERSION", PaymentCoreBuildInfo.version)
}
