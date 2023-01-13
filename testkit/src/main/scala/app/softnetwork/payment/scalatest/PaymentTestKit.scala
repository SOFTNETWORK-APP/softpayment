package app.softnetwork.payment.scalatest

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, Multipart, StatusCodes}
import akka.http.scaladsl.server.Route
import app.softnetwork.api.server.config.ServerSettings.RootPath
import app.softnetwork.payment.api.PaymentGrpcServices
import app.softnetwork.payment.config.PaymentSettings._
import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.payment.launch.{PaymentGuardian, PaymentRoutes}
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
import app.softnetwork.payment.service.{GenericPaymentService, MockPaymentService}
import app.softnetwork.persistence.launch.PersistentEntity
import app.softnetwork.persistence.query.{EventProcessorStream, InMemoryJournalProvider}
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.scalatest.SchedulerTestKit
import app.softnetwork.session.scalatest.{SessionServiceRoute, SessionTestKit}
import org.scalatest.Suite

import java.nio.file.Paths
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
    new GenericPaymentCommandProcessorStream with MockPaymentHandler with InMemoryJournalProvider {
      override val forTests: Boolean = true
      override implicit def system: ActorSystem[_] = sys
    }

  override def scheduler2PaymentProcessorStream
    : ActorSystem[_] => Scheduler2PaymentProcessorStream = sys =>
    new Scheduler2PaymentProcessorStream with MockPaymentHandler with InMemoryJournalProvider {
      override val tag: String = SchedulerSettings.tag(MockPaymentBehavior.persistenceId)
      override val forTests: Boolean = true
      override implicit def system: ActorSystem[_] = sys
    }

  def payInFor3DS(orderUuid: String, transactionId: String, registerCard: Boolean)(implicit
    system: ActorSystem[_]
  ): Future[Either[PayInFailed, Either[PaymentRedirection, PaidIn]]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? PayInFor3DS(orderUuid, transactionId, registerCard) map {
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
    registerCard: Boolean = true
  )(implicit
    system: ActorSystem[_]
  ): Future[Either[CardPreAuthorizationFailed, Either[PaymentRedirection, CardPreAuthorized]]] = {
    implicit val ec: ExecutionContextExecutor = system.executionContext
    MockPaymentHandler !? PreAuthorizeCardFor3DS(orderUuid, preAuthorizationId, registerCard) map {
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

  override def entities: ActorSystem[_] => Seq[PersistentEntity[_, _, _, _]] = sys =>
    schedulerEntities(sys) ++ sessionEntities(sys) ++ paymentEntities(sys)

  override def eventProcessorStreams: ActorSystem[_] => Seq[EventProcessorStream[_]] = sys =>
    schedulerEventProcessorStreams(sys) ++
    paymentEventProcessorStreams(sys)

  /*override def initSystem: ActorSystem[_] => Unit = system => {
    initSchedulerSystem(system)
  }*/
}

trait PaymentRouteTestKit
    extends SessionTestKit
    with PaymentTestKit
    with PaymentRoutes
    with PaymentGrpcServices {
  _: Suite =>

  import app.softnetwork.serialization._

  override def paymentService: ActorSystem[_] => GenericPaymentService = system =>
    MockPaymentService(system)

  override lazy val additionalConfig: String = paymentGrpcConfig

  override def apiRoutes(system: ActorSystem[_]): Route =
    paymentService(system).route ~
    SessionServiceRoute(system).route

  def loadPaymentAccount(): PaymentAccountView = {
    withCookies(
      Get(s"/$RootPath/$PaymentPath")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[PaymentAccountView]
    }
  }

  def loadCards(): Seq[Card] = {
    withCookies(
      Get(s"/$RootPath/$PaymentPath/$CardRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[Seq[Card]]
    }
  }

  def loadBankAccount(): BankAccountView = {
    withCookies(
      Get(s"/$RootPath/$PaymentPath/$BankRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[BankAccountView]
    }
  }

  def addKycDocuments(): Unit = {
    val path =
      Paths.get(Thread.currentThread().getContextClassLoader.getResource("avatar.png").getPath)
    loadPaymentAccount().documents
      .filter(_.documentStatus == KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED)
      .map(_.documentType)
      .foreach { documentType =>
        withCookies(
          Post(
            s"/$RootPath/$PaymentPath/$KycRoute?documentType=$documentType",
            entity = Multipart.FormData
              .fromPath(
                "pages",
                ContentTypes.`application/octet-stream`,
                path,
                100000
              )
              .toEntity
          ).withHeaders(
            RawHeader("Content-Type", "application/x-www-form-urlencoded"),
            RawHeader("Content-Type", "multipart/form-data")
          )
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          assert(
            loadKycDocumentStatus(
              documentType
            ).status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED
          )
        }
      }
  }

  def loadKycDocumentStatus(
    documentType: KycDocument.KycDocumentType
  ): KycDocumentValidationReport = {
    withCookies(
      Get(s"/$RootPath/$PaymentPath/$KycRoute?documentType=$documentType")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[KycDocumentValidationReport]
    }
  }

  def validateKycDocuments(): Unit = {
    loadPaymentAccount().documents
      .filter(_.documentStatus == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
      .map(_.documentType)
      .foreach { documentType =>
        withCookies(
          Get(s"/$RootPath/$PaymentPath/$KycRoute?documentType=$documentType")
        ) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val report = responseAs[KycDocumentValidationReport]
          assert(report.status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
          Get(
            s"/$RootPath/$PaymentPath/$HooksRoute?EventType=KYC_SUCCEEDED&RessourceId=${report.id}"
          ) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
            assert(
              loadKycDocumentStatus(
                documentType
              ).status == KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
            )
          }
        }
      }
  }

  def loadDeclaration(): UboDeclarationView = {
    withCookies(
      Get(s"/$RootPath/$PaymentPath/$DeclarationRoute")
    ) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[UboDeclarationView]
    }
  }
}
