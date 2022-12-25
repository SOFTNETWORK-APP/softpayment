package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  BankAccountLoaded,
  CancelMandate,
  CancelPreAuthorization,
  CreateOrUpdatePaymentAccount,
  DirectDebit,
  DirectDebitFailed,
  DirectDebited,
  LoadBankAccount,
  LoadDirectDebitTransaction,
  MandateCanceled,
  PaidIn,
  PaidOut,
  PayInFailed,
  PayInWithCardPreAuthorized,
  PayOut,
  PayOutFailed,
  PaymentAccountCreated,
  PaymentAccountUpdated,
  PaymentCommand,
  PaymentError,
  PreAuthorizationCanceled,
  RecurringPaymentRegistered,
  Refund,
  RefundFailed,
  Refunded,
  RegisterRecurringPayment,
  Transfer,
  TransferFailed,
  Transferred
}
import app.softnetwork.payment.model.RecurringPayment
import app.softnetwork.payment.serialization._
import app.softnetwork.persistence.typed.CommandTypeKey

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.implicitConversions

trait PaymentServer extends PaymentServiceApi with GenericPaymentHandler {
  _: CommandTypeKey[PaymentCommand] =>
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
    !?(PayInWithCardPreAuthorized(preAuthorizationId, creditedAccount)) map {
      case r: PaidIn =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case f: PayInFailed =>
        TransactionResponse(
          transactionId = Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
      case e: PaymentError =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some(e.message)
        )
      case _ =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("unknown")
        )
    }
  }

  override def cancelPreAuthorization(
    in: CancelPreAuthorizationRequest
  ): Future[CancelPreAuthorizationResponse] = {
    import in._
    !?(CancelPreAuthorization(orderUuid, cardPreAuthorizedTransactionId)) map {
      case r: PreAuthorizationCanceled =>
        CancelPreAuthorizationResponse(Some(r.preAuthorizationCanceled))
      case _ => CancelPreAuthorizationResponse()
    }
  }

  override def refund(in: RefundRequest): Future[TransactionResponse] = {
    import in._
    !?(
      Refund(
        orderUuid,
        payInTransactionId,
        refundAmount,
        currency,
        reasonMessage,
        initializedByClient
      )
    ) map {
      case r: Refunded =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case f: RefundFailed =>
        TransactionResponse(
          transactionId = Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
      case e: PaymentError =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some(e.message)
        )
      case _ =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("unknown")
        )
    }
  }

  override def payOut(in: PayOutRequest): Future[TransactionResponse] = {
    import in._
    !?(PayOut(orderUuid, creditedAccount, creditedAmount, feesAmount, currency)) map {
      case r: PaidOut =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case f: PayOutFailed =>
        TransactionResponse(
          transactionId = Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
      case e: PaymentError =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some(e.message)
        )
      case _ =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("unknown")
        )
    }
  }

  override def transfer(in: TransferRequest): Future[TransferResponse] = {
    import in._
    !?(
      Transfer(
        orderUuid,
        debitedAccount,
        creditedAccount,
        debitedAmount,
        feesAmount,
        currency,
        payOutRequired,
        externalReference
      )
    ) map {
      case r: Transferred =>
        TransferResponse(
          Some(r.transferredTransactionId),
          r.transferredTransactionStatus,
          r.paidOutTransactionId
        )
      case f: TransferFailed =>
        TransferResponse(
          Some(f.transferredTransactionId),
          f.transferredTransactionStatus,
          error = Some(f.message)
        )
      case e: PaymentError =>
        TransferResponse(
          transferredTransactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some(e.message)
        )
      case _ =>
        TransferResponse(
          transferredTransactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("unknown")
        )
    }
  }

  override def directDebit(in: DirectDebitRequest): Future[TransactionResponse] = {
    import in._
    !?(
      DirectDebit(
        creditedAccount,
        debitedAmount,
        feesAmount,
        currency,
        statementDescriptor,
        externalReference
      )
    ) map {
      case r: DirectDebited =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case f: DirectDebitFailed =>
        TransactionResponse(
          transactionId = Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
      case e: PaymentError =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some(e.message)
        )
      case _ =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("unknown")
        )
    }
  }

  override def loadDirectDebitTransaction(
    in: LoadDirectDebitTransactionRequest
  ): Future[TransactionResponse] = {
    import in._
    !?(LoadDirectDebitTransaction(directDebitTransactionId)) map {
      case r: DirectDebited =>
        TransactionResponse(
          transactionId = Some(r.transactionId),
          transactionStatus = r.transactionStatus
        )
      case f: DirectDebitFailed =>
        TransactionResponse(
          transactionId = Some(f.transactionId),
          transactionStatus = f.transactionStatus,
          error = Some(f.message)
        )
      case e: PaymentError =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some(e.message)
        )
      case _ =>
        TransactionResponse(
          transactionStatus = TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          error = Some("unknown")
        )
    }
  }

  override def registerRecurringPayment(
    in: RegisterRecurringPaymentRequest
  ): Future[RegisterRecurringPaymentResponse] = {
    import in._
    val maybeType: Option[RecurringPayment.RecurringPaymentType] = `type`
    maybeType match {
      case Some(atype) =>
        !?(
          RegisterRecurringPayment(
            debitedAccount,
            firstDebitedAmount,
            firstFeesAmount,
            currency,
            atype,
            startDate,
            endDate,
            frequency,
            fixedNextAmount,
            nextDebitedAmount,
            nextFeesAmount
          )
        ) map {
          case r: RecurringPaymentRegistered =>
            RegisterRecurringPaymentResponse(Some(r.recurringPaymentRegistrationId))
          case _ => RegisterRecurringPaymentResponse()
        }
      case _ => Future.successful(RegisterRecurringPaymentResponse())
    }
  }

  override def cancelMandate(in: CancelMandateRequest): Future[CancelMandateResponse] = {
    import in._
    !?(CancelMandate(externalUuid)) map {
      case MandateCanceled => CancelMandateResponse(true)
      case _               => CancelMandateResponse()
    }
  }

  override def loadBankAccountOwner(
    in: LoadBankAccountOwnerRequest
  ): Future[LoadBankAccountOwnerResponse] = {
    import in._
    !?(LoadBankAccount(externalUuid)) map {
      case r: BankAccountLoaded =>
        LoadBankAccountOwnerResponse(r.bankAccount.ownerName, Some(r.bankAccount.ownerAddress))
      case _ => LoadBankAccountOwnerResponse()
    }
  }
}
