package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.persistence.query.AccountEventProcessorStreams.InternalAccountEvents2AccountProcessorStream
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.handlers.MockSoftPayAccountHandler
import app.softnetwork.payment.launch.SoftPayGuardian
import app.softnetwork.payment.persistence.typed.MockSoftPayAccountBehavior
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.{
  EventProcessorStream,
  InMemoryJournalProvider,
  InMemoryOffsetProvider
}
import org.scalatest.Suite
import org.slf4j.{Logger, LoggerFactory}

trait SoftPayTestKit extends PaymentTestKit with SoftPayGuardian {
  _: Suite =>

  /** @return
    *   roles associated with this node
    */
  override def roles: Seq[String] = super.roles :+ AkkaNodeRole :+ AccountSettings.AkkaNodeRole

  override def internalAccountEvents2AccountProcessorStream
    : ActorSystem[_] => InternalAccountEvents2AccountProcessorStream = sys =>
    new InternalAccountEvents2AccountProcessorStream
      with MockSoftPayAccountHandler
      with InMemoryJournalProvider
      with InMemoryOffsetProvider {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override def tag: String = s"${MockSoftPayAccountBehavior.persistenceId}-to-internal"
      override lazy val forTests: Boolean = true
      override implicit def system: ActorSystem[_] = sys
    }

  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    schedulerEntities(sys) ++ sessionEntities(sys) ++ accountEntities(sys) ++ paymentEntities(
      sys
    ) ++ notificationEntities(sys)

  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    schedulerEventProcessorStreams(sys) ++
    paymentEventProcessorStreams(sys) ++
    accountEventProcessorStreams(sys) ++
    notificationEventProcessorStreams(sys)

  override def initSystem: ActorSystem[_] => Unit = system => {
    initAccountSystem(system)
    initSchedulerSystem(system)
    registerProvidersAccount(system)
  }

}
