package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.serialization._
import app.softnetwork.payment.handlers.PaymentDao
import app.softnetwork.payment.message.PaymentMessages.{
  CreateOrUpdatePaymentAccount,
  DirectDebitFailed,
  DirectDebited,
  PaidIn,
  PaidOut,
  PayOutFailed,
  PaymentAccountCreated,
  PaymentAccountUpdated,
  PreAuthorizationCanceled,
  RecurringPaymentRegistered,
  RefundFailed,
  Refunded,
  TransferFailed,
  Transferred
}
import app.softnetwork.payment.model.{BankAccount, PaymentAccount, RecurringPayment, Transaction}
import app.softnetwork.payment.serialization._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.implicitConversions

trait PaymentServer extends PaymentServiceApi with PaymentDao {
  implicit def system: ActorSystem[_]

  implicit lazy val ec: ExecutionContextExecutor = system.executionContext

  override def createOrUpdatePaymentAccount(
    in: CreateOrUpdatePaymentAccountRequest
  ): Future[CreateOrUpdatePaymentAccountResponse] = {
    in.paymentAccount match {
      case Some(paymentAccount) =>
        !?(CreateOrUpdatePaymentAccount(paymentAccount)) map {
          case PaymentAccountCreated | PaymentAccountUpdated =>
            CreateOrUpdatePaymentAccountResponse(true)
          case _ => CreateOrUpdatePaymentAccountResponse()
        }
      case _ => Future.successful(CreateOrUpdatePaymentAccountResponse())
    }
  }

  override def payInWithCardPreAuthorized(
    in: PayInWithCardPreAuthorizedRequest
  ): Future[TransactionResponse] = {
    import in._
    payInWithCardPreAuthorized(
      preAuthorizationId,
      creditedAccount,
      debitedAmount,
      Some(clientId)
    ) map {
      case Right(r: PaidIn) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case Left(f) =>
        TransactionResponse(
          transactionId = if (f.transactionId.isEmpty) None else Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
    }
  }

  override def cancelPreAuthorization(
    in: CancelPreAuthorizationRequest
  ): Future[CancelPreAuthorizationResponse] = {
    import in._
    cancelPreAuthorization(orderUuid, cardPreAuthorizedTransactionId, Some(clientId)) map {
      case Right(r: PreAuthorizationCanceled) =>
        CancelPreAuthorizationResponse(Some(r.preAuthorizationCanceled))
      case _ => CancelPreAuthorizationResponse()
    }
  }

  override def refund(in: RefundRequest): Future[TransactionResponse] = {
    import in._
    refund(
      orderUuid,
      payInTransactionId,
      refundAmount,
      currency,
      reasonMessage,
      initializedByClient,
      Some(clientId)
    ) map {
      case Right(r: Refunded) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case Left(f: RefundFailed) =>
        TransactionResponse(
          transactionId = if (f.transactionId.isEmpty) None else Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
    }
  }

  override def payOut(in: PayOutRequest): Future[TransactionResponse] = {
    import in._
    payOut(
      orderUuid,
      creditedAccount,
      creditedAmount,
      feesAmount,
      currency,
      externalReference,
      payInTransactionId,
      Some(clientId)
    ) map {
      case Right(r: PaidOut) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case Left(f: PayOutFailed) =>
        TransactionResponse(
          transactionId = if (f.transactionId.isEmpty) None else Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
    }
  }

  override def transfer(in: TransferRequest): Future[TransferResponse] = {
    import in._
    transfer(
      orderUuid,
      debitedAccount,
      creditedAccount,
      debitedAmount,
      feesAmount,
      currency,
      payOutRequired,
      externalReference,
      Some(clientId)
    ) map {
      case Right(r: Transferred) =>
        TransferResponse(
          Some(r.transferredTransactionId),
          r.transferredTransactionStatus,
          r.paidOutTransactionId
        )
      case Left(f: TransferFailed) =>
        TransferResponse(
          if (f.transferredTransactionId.isEmpty) None else Some(f.transferredTransactionId),
          f.transferredTransactionStatus,
          error = Some(f.message)
        )
    }
  }

  override def directDebit(in: DirectDebitRequest): Future[TransactionResponse] = {
    import in._
    directDebit(
      creditedAccount,
      debitedAmount,
      feesAmount,
      currency,
      statementDescriptor,
      externalReference,
      Some(clientId)
    ) map {
      case Right(r: DirectDebited) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case Left(f: DirectDebitFailed) =>
        TransactionResponse(
          transactionId = if (f.transactionId.isEmpty) None else Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
    }
  }

  override def loadDirectDebitTransaction(
    in: LoadDirectDebitTransactionRequest
  ): Future[TransactionResponse] = {
    import in._
    loadDirectDebitTransaction(directDebitTransactionId, Some(clientId)) map {
      case Right(r: DirectDebited) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case Left(f: DirectDebitFailed) =>
        TransactionResponse(
          transactionId = if (f.transactionId.isEmpty) None else Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
    }
  }

  override def registerRecurringPayment(
    in: RegisterRecurringPaymentRequest
  ): Future[RegisterRecurringPaymentResponse] = {
    import in._
    val maybeType: Option[RecurringPayment.RecurringPaymentType] = `type`
    maybeType match {
      case Some(recurringPaymentType) =>
        registerRecurringPayment(
          debitedAccount,
          firstDebitedAmount,
          firstFeesAmount,
          currency,
          recurringPaymentType,
          startDate,
          endDate,
          frequency,
          fixedNextAmount,
          nextDebitedAmount,
          nextFeesAmount,
          statementDescriptor,
          externalReference,
          Some(clientId)
        ) map {
          case Right(r: RecurringPaymentRegistered) =>
            RegisterRecurringPaymentResponse(Some(r.recurringPaymentRegistrationId))
          case _ => RegisterRecurringPaymentResponse()
        }
      case _ => Future.successful(RegisterRecurringPaymentResponse())
    }
  }

  override def cancelMandate(in: CancelMandateRequest): Future[CancelMandateResponse] = {
    import in._
    cancelMandate(externalUuid, Some(clientId)) map {
      case Right(r) => CancelMandateResponse(r)
      case Left(_)  => CancelMandateResponse()
    }
  }

  override def loadBankAccountOwner(
    in: LoadBankAccountOwnerRequest
  ): Future[LoadBankAccountOwnerResponse] = {
    import in._
    loadBankAccount(externalUuid, Some(clientId)) map {
      case Some(r: BankAccount) =>
        LoadBankAccountOwnerResponse(r.ownerName, Some(r.ownerAddress))
      case _ => LoadBankAccountOwnerResponse()
    }
  }

  override def loadLegalUser(
    in: LoadLegalUserRequest
  ): Future[LoadLegalUserResponse] = {
    import in._
    loadPaymentAccount(externalUuid, Some(clientId)) map {
      case Some(r: PaymentAccount) if r.user.isLegalUser =>
        val legalUser = r.getLegalUser
        LoadLegalUserResponse(
          legalUser.legalUserType,
          legalUser.legalName,
          legalUser.siret,
          Some(legalUser.legalRepresentativeAddress),
          Some(legalUser.headQuartersAddress)
        )
      case Some(r: PaymentAccount) if r.bankAccount.isDefined =>
        val bankAccount = r.getBankAccount
        LoadLegalUserResponse(
          LegalUserType.SOLETRADER,
          bankAccount.ownerName,
          "",
          Some(bankAccount.ownerAddress),
          None
        )
      case _ => LoadLegalUserResponse()
    }
  }

  override def loadPayInTransaction(
    in: LoadPayInTransactionRequest
  ): Future[TransactionResponse] = {
    import in._
    loadPayInTransaction(orderUuid, payInTransactionId, Some(clientId)) map {
      case Right(r) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case Left(_) =>
        TransactionResponse(
          transactionId = None,
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("transaction not found")
        )
    }
  }

  override def loadPayOutTransaction(
    in: LoadPayOutTransactionRequest
  ): Future[TransactionResponse] = {
    import in._
    loadPayOutTransaction(orderUuid, payOutTransactionId, Some(clientId)) map {
      case Right(r) =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case _ =>
        TransactionResponse(
          transactionId = None,
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("transaction nont found")
        )
    }
  }
}

object PaymentServer {
  def apply(sys: ActorSystem[_]): PaymentServer = {
    new PaymentServer {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit val system: ActorSystem[_] = sys
    }
  }
}
