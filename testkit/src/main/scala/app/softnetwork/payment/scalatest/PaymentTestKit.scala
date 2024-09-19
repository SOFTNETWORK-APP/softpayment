package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.notification.scalatest.AllNotificationsTestKit
import app.softnetwork.payment.api._
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig._
import app.softnetwork.payment.handlers.{
  MockPaymentHandler,
  MockSoftPayAccountDao,
  SoftPayAccountDao
}
import app.softnetwork.payment.launch.PaymentGuardian
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model._
import app.softnetwork.payment.persistence.query.{
  PaymentCommandProcessorStream,
  Scheduler2PaymentProcessorStream
}
import app.softnetwork.payment.persistence.typed.{
  MockPaymentBehavior,
  MockSoftPayAccountBehavior,
  PaymentBehavior,
  SoftPayAccountBehavior
}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.{
  EventProcessorStream,
  InMemoryJournalProvider,
  InMemoryOffsetProvider
}
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.scalatest.SchedulerTestKit
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.Suite
import org.slf4j.{Logger, LoggerFactory}
import org.softnetwork.session.model.ApiKey

import scala.concurrent.{ExecutionContext, Future}

trait PaymentTestKit
    extends SchedulerTestKit
    with AllNotificationsTestKit
    with PaymentGuardian
    with PaymentProviderTestKit {
  _: Suite =>

  /** @return
    *   roles associated with this node
    */
  override def roles: Seq[String] = super.roles :+ akkaNodeRole :+ AccountSettings.AkkaNodeRole

  override def paymentBehavior: ActorSystem[_] => PaymentBehavior = _ => MockPaymentBehavior

  override def softPayAccountBehavior: ActorSystem[_] => SoftPayAccountBehavior = _ =>
    MockSoftPayAccountBehavior

  override def paymentServer: ActorSystem[_] => PaymentServer = system => MockPaymentServer(system)

  override def clientServer: ActorSystem[_] => ClientServer = system => MockClientServer(system)

  def loadApiKey(clientId: String): Future[Option[ApiKey]] =
    MockPaymentBehavior.softPayAccountDao.loadApiKey(clientId)

  def clientId: String = provider.clientId

  override lazy val config: Config = akkaConfig
    .withFallback(ConfigFactory.load("softnetwork-in-memory-persistence.conf"))
    .withFallback(
      ConfigFactory.parseString(providerSettings)
    )
    .withFallback(ConfigFactory.load())

  override def paymentCommandProcessorStream: ActorSystem[_] => PaymentCommandProcessorStream =
    sys =>
      new PaymentCommandProcessorStream
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

  def payInCallback(
    orderUuid: String,
    transactionId: String,
    registerCard: Boolean,
    printReceipt: Boolean
  )(implicit
    ec: ExecutionContext
  ): Future[Either[PayInFailed, Either[PaymentRedirection, PaidIn]]] = {
    MockPaymentHandler !? PayInCallback(orderUuid, transactionId, registerCard, printReceipt) map {
      case result: PaymentRedirection => Right(Left(result))
      case result: PaidIn             => Right(Right(result))
      case error: PayInFailed         => Left(error)
      case _ =>
        Left(PayInFailed("", Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED, "unknown"))
    }
  }

  def preAuthorizeCardCallback(
    orderUuid: String,
    preAuthorizationId: String,
    registerCard: Boolean = true,
    printReceipt: Boolean = false
  )(implicit
    ec: ExecutionContext
  ): Future[Either[PreAuthorizationFailed, Either[PaymentRedirection, PaymentPreAuthorized]]] = {
    MockPaymentHandler !? PreAuthorizeCallback(
      orderUuid,
      preAuthorizationId,
      registerCard,
      printReceipt
    ) map {
      case result: PaymentRedirection    => Right(Left(result))
      case result: PaymentPreAuthorized  => Right(Right(result))
      case error: PreAuthorizationFailed => Left(error)
      case _                             => Left(PreAuthorizationFailed("unknown"))
    }
  }

  def payInFirstRecurringPaymentCallback(
    recurringPayInRegistrationId: String,
    transactionId: String
  )(implicit ec: ExecutionContext): Future[
    Either[FirstRecurringCardPaymentFailed, Either[PaymentRedirection, FirstRecurringPaidIn]]
  ] = {
    MockPaymentHandler !? FirstRecurringPaymentCallback(
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
  )(implicit ec: ExecutionContext): Future[Either[PaymentError, KycDocumentStatusUpdated]] = {
    MockPaymentHandler !? UpdateKycDocumentStatus(kycDocumentId, status) map {
      case result: KycDocumentStatusUpdated => Right(result)
      case error: PaymentError              => Left(error)
      case _                                => Left(KycDocumentStatusNotUpdated)
    }
  }

  def updateUboDeclarationStatus(
    uboDeclarationId: String,
    status: Option[UboDeclaration.UboDeclarationStatus] = None
  )(implicit ec: ExecutionContext): Future[Boolean] = {
    MockPaymentHandler !? UpdateUboDeclarationStatus(uboDeclarationId, status) map {
      case UboDeclarationStatusUpdated => true
      case _                           => false
    }
  }

  def updateMandateStatus(
    mandateId: String,
    status: Option[Mandate.MandateStatus] = None
  )(implicit ec: ExecutionContext): Future[Either[PaymentError, MandateStatusUpdated]] = {
    MockPaymentHandler !? UpdateMandateStatus(mandateId, status) map {
      case result: MandateStatusUpdated => Right(result)
      case error: PaymentError          => Left(error)
      case _                            => Left(MandateStatusNotUpdated)
    }
  }

  def validateRegularUser(userId: String)(implicit ec: ExecutionContext): Future[Boolean] = {
    MockPaymentHandler !? ValidateRegularUser(userId) map {
      case RegularUserValidated => true
      case _                    => false
    }
  }

  def invalidateRegularUser(userId: String)(implicit ec: ExecutionContext): Future[Boolean] = {
    MockPaymentHandler !? InvalidateRegularUser(userId) map {
      case RegularUserInvalidated => true
      case _                      => false
    }
  }

  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    schedulerEntities(sys) ++ sessionEntities(sys) ++ paymentEntities(
      sys
    ) ++ notificationEntities(sys)

  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    schedulerEventProcessorStreams(sys) ++
    paymentEventProcessorStreams(sys) ++
    notificationEventProcessorStreams(sys)

  override def initSystem: ActorSystem[_] => Unit = system => {
    initSchedulerSystem(system)
    registerProvidersAccount(system)
  }

  override def softPayAccountDao: SoftPayAccountDao = MockSoftPayAccountDao
}
