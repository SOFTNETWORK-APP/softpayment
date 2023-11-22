package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.account.handlers.AccountDao
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.account.persistence.query.AccountEventProcessorStreams.InternalAccountEvents2AccountProcessorStream
import app.softnetwork.account.persistence.typed.AccountBehavior
import app.softnetwork.notification.scalatest.AllNotificationsTestKit
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.handlers.{
  MockPaymentHandler,
  MockSoftPaymentAccountDao,
  MockSoftPaymentAccountHandler
}
import app.softnetwork.payment.launch.PaymentGuardian
import app.softnetwork.payment.message.PaymentMessages.{
  KycDocumentStatusNotUpdated,
  UboDeclarationStatusUpdated,
  _
}
import app.softnetwork.payment.model._
import app.softnetwork.payment.persistence.query.{
  GenericPaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{
  GenericPaymentBehavior,
  MockPaymentBehavior,
  MockSoftPaymentAccountBehavior
}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.{
  EventProcessorStream,
  InMemoryJournalProvider,
  InMemoryOffsetProvider
}
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.scalatest.SchedulerTestKit
import app.softnetwork.session.service.SessionMaterials
import org.scalatest.Suite
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

trait PaymentTestKit extends SchedulerTestKit with PaymentGuardian with AllNotificationsTestKit {
  _: Suite with SessionMaterials =>

  /** @return
    *   roles associated with this node
    */
  override def roles: Seq[String] = super.roles :+ AkkaNodeRole :+ AccountSettings.AkkaNodeRole

  override def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior = _ =>
    MockPaymentBehavior

  override def accountBehavior
    : ActorSystem[_] => AccountBehavior[SoftPaymentAccount, BasicAccountProfile] = _ =>
    MockSoftPaymentAccountBehavior

  override def accountDao: AccountDao = MockSoftPaymentAccountDao

  override def paymentCommandProcessorStream
    : ActorSystem[_] => GenericPaymentCommandProcessorStream = sys =>
    new GenericPaymentCommandProcessorStream
      with MockPaymentHandler
      with InMemoryJournalProvider
      with InMemoryOffsetProvider {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override val forTests: Boolean = true
      override implicit def system: ActorSystem[_] = sys
    }

  override def scheduler2PaymentProcessorStream
    : ActorSystem[_] => Scheduler2PaymentProcessorStream = sys =>
    new Scheduler2PaymentProcessorStream
      with MockPaymentHandler
      with InMemoryJournalProvider
      with InMemoryOffsetProvider {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override val tag: String = SchedulerSettings.tag(MockPaymentBehavior.persistenceId)
      override val forTests: Boolean = true
      override implicit def system: ActorSystem[_] = sys
    }

  override def internalAccountEvents2AccountProcessorStream
    : ActorSystem[_] => InternalAccountEvents2AccountProcessorStream = sys =>
    new InternalAccountEvents2AccountProcessorStream
      with MockSoftPaymentAccountHandler
      with InMemoryJournalProvider
      with InMemoryOffsetProvider {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override def tag: String = s"${MockSoftPaymentAccountBehavior.persistenceId}-to-internal"
      override lazy val forTests: Boolean = true
      override implicit def system: ActorSystem[_] = sys
    }

  def payInFor3DS(
    orderUuid: String,
    transactionId: String,
    registerCard: Boolean,
    printReceipt: Boolean
  ): Future[Either[PayInFailed, Either[PaymentRedirection, PaidIn]]] = {
    MockPaymentHandler !? PayInFor3DS(orderUuid, transactionId, registerCard, printReceipt) map {
      case result: PaymentRedirection => Right(Left(result))
      case result: PaidIn             => Right(Right(result))
      case error: PayInFailed         => Left(error)
      case _ =>
        Left(PayInFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
    }
  }

  def preAuthorizeCardFor3DS(
    orderUuid: String,
    preAuthorizationId: String,
    registerCard: Boolean = true,
    printReceipt: Boolean = false
  ): Future[Either[CardPreAuthorizationFailed, Either[PaymentRedirection, CardPreAuthorized]]] = {
    MockPaymentHandler !? PreAuthorizeCardFor3DS(
      orderUuid,
      preAuthorizationId,
      registerCard,
      printReceipt
    ) map {
      case result: PaymentRedirection        => Right(Left(result))
      case result: CardPreAuthorized         => Right(Right(result))
      case error: CardPreAuthorizationFailed => Left(error)
      case _                                 => Left(CardPreAuthorizationFailed("unknown"))
    }
  }

  def payInFirstRecurringFor3DS(
    recurringPayInRegistrationId: String,
    transactionId: String
  ): Future[
    Either[FirstRecurringCardPaymentFailed, Either[PaymentRedirection, FirstRecurringPaidIn]]
  ] = {
    MockPaymentHandler !? PayInFirstRecurringFor3DS(
      recurringPayInRegistrationId,
      transactionId
    ) map {
      case result: PaymentRedirection             => Right(Left(result))
      case result: FirstRecurringPaidIn           => Right(Right(result))
      case error: FirstRecurringCardPaymentFailed => Left(error)
      case _ =>
        Left(
          FirstRecurringCardPaymentFailed(
            "",
            Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
            "unknown"
          )
        )
    }
  }

  def updateKycDocumentStatus(
    kycDocumentId: String,
    status: Option[KycDocument.KycDocumentStatus] = None
  ): Future[Either[PaymentError, KycDocumentStatusUpdated]] = {
    MockPaymentHandler !? UpdateKycDocumentStatus(kycDocumentId, status) map {
      case result: KycDocumentStatusUpdated => Right(result)
      case error: PaymentError              => Left(error)
      case _                                => Left(KycDocumentStatusNotUpdated)
    }
  }

  def updateUboDeclarationStatus(
    uboDeclarationId: String,
    status: Option[UboDeclaration.UboDeclarationStatus] = None
  ): Future[Boolean] = {
    MockPaymentHandler !? UpdateUboDeclarationStatus(uboDeclarationId, status) map {
      case UboDeclarationStatusUpdated => true
      case _                           => false
    }
  }

  def updateMandateStatus(
    mandateId: String,
    status: Option[BankAccount.MandateStatus] = None
  ): Future[Either[PaymentError, MandateStatusUpdated]] = {
    MockPaymentHandler !? UpdateMandateStatus(mandateId, status) map {
      case result: MandateStatusUpdated => Right(result)
      case error: PaymentError          => Left(error)
      case _                            => Left(MandateStatusNotUpdated)
    }
  }

  def validateRegularUser(userId: String): Future[Boolean] = {
    MockPaymentHandler !? ValidateRegularUser(userId) map {
      case RegularUserValidated => true
      case _                    => false
    }
  }

  def invalidateRegularUser(userId: String): Future[Boolean] = {
    MockPaymentHandler !? InvalidateRegularUser(userId) map {
      case RegularUserInvalidated => true
      case _                      => false
    }
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
  }
}
