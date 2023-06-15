package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.handlers.MockPaymentHandler
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
import app.softnetwork.payment.persistence.typed.{GenericPaymentBehavior, MockPaymentBehavior}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.{
  EventProcessorStream,
  InMemoryJournalProvider,
  InMemoryOffsetProvider
}
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.scalatest.SchedulerTestKit
import org.scalatest.Suite
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}

trait PaymentTestKit extends SchedulerTestKit with PaymentGuardian { _: Suite =>

  /** @return
    *   roles associated with this node
    */
  override def roles: Seq[String] = super.roles :+ AkkaNodeRole

  override def paymentAccountBehavior: ActorSystem[_] => GenericPaymentBehavior = _ =>
    MockPaymentBehavior

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

  def payInFor3DS(
    orderUuid: String,
    transactionId: String,
    registerCard: Boolean,
    printReceipt: Boolean
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[PayInFailed, Either[PaymentRedirection, PaidIn]]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
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
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[CardPreAuthorizationFailed, Either[PaymentRedirection, CardPreAuthorized]]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
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

  def payInFirstRecurringFor3DS(recurringPayInRegistrationId: String, transactionId: String)(
    implicit system: ActorSystem[_]
  ): Future[
    Either[FirstRecurringCardPaymentFailed, Either[PaymentRedirection, FirstRecurringPaidIn]]
  ] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
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
  )(implicit system: ActorSystem[_]): Future[Either[PaymentError, KycDocumentStatusUpdated]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? UpdateKycDocumentStatus(kycDocumentId, status) map {
      case result: KycDocumentStatusUpdated => Right(result)
      case error: PaymentError              => Left(error)
      case _                                => Left(KycDocumentStatusNotUpdated)
    }
  }

  def updateUboDeclarationStatus(
    uboDeclarationId: String,
    status: Option[UboDeclaration.UboDeclarationStatus] = None
  )(implicit system: ActorSystem[_]): Future[Boolean] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? UpdateUboDeclarationStatus(uboDeclarationId, status) map {
      case UboDeclarationStatusUpdated => true
      case _                           => false
    }
  }

  def updateMandateStatus(mandateId: String, status: Option[BankAccount.MandateStatus] = None)(
    implicit system: ActorSystem[_]
  ): Future[Either[PaymentError, MandateStatusUpdated]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? UpdateMandateStatus(mandateId, status) map {
      case result: MandateStatusUpdated => Right(result)
      case error: PaymentError          => Left(error)
      case _                            => Left(MandateStatusNotUpdated)
    }
  }

  def validateRegularUser(userId: String)(implicit system: ActorSystem[_]): Future[Boolean] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? ValidateRegularUser(userId) map {
      case RegularUserValidated => true
      case _                    => false
    }
  }

  def invalidateRegularUser(userId: String)(implicit system: ActorSystem[_]): Future[Boolean] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? InvalidateRegularUser(userId) map {
      case RegularUserInvalidated => true
      case _                      => false
    }
  }

  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    schedulerEntities(sys) ++ sessionEntities(sys) ++ paymentEntities(sys)

  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    schedulerEventProcessorStreams(sys) ++
    paymentEventProcessorStreams(sys)

  /*override def initSystem: ActorSystem[_] => Unit = system => {
    initSchedulerSystem(system)
  }*/
}
