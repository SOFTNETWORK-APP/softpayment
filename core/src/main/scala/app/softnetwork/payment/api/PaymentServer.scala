package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.api.serialization._
import app.softnetwork.payment.handlers.PaymentHandler
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
  LoadPaymentAccount,
  MandateCanceled,
  PaidIn,
  PaidOut,
  PayInFailed,
  PayInWithCardPreAuthorized,
  PayOut,
  PayOutFailed,
  PaymentAccountCreated,
  PaymentAccountLoaded,
  PaymentAccountUpdated,
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
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.implicitConversions

trait PaymentServer extends PaymentServiceApi with PaymentHandler {
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
    !?(
      PayInWithCardPreAuthorized(preAuthorizationId, creditedAccount, debitedAmount, Some(clientId))
    ) map {
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
    !?(CancelPreAuthorization(orderUuid, cardPreAuthorizedTransactionId, Some(clientId))) map {
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
        initializedByClient,
        Some(clientId)
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
    !?(
      PayOut(
        orderUuid,
        creditedAccount,
        creditedAmount,
        feesAmount,
        currency,
        externalReference,
        Some(clientId)
      )
    ) map {
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
        externalReference,
        Some(clientId)
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
        externalReference,
        Some(clientId)
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
    !?(LoadDirectDebitTransaction(directDebitTransactionId, Some(clientId))) map {
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
            nextFeesAmount,
            statementDescriptor,
            externalReference,
            Some(clientId)
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
    !?(LoadBankAccount(externalUuid, Some(clientId))) map {
      case r: BankAccountLoaded =>
        LoadBankAccountOwnerResponse(r.bankAccount.ownerName, Some(r.bankAccount.ownerAddress))
      case _ => LoadBankAccountOwnerResponse()
    }
  }

  override def loadLegalUser(
    in: LoadLegalUserRequest
  ): Future[LoadLegalUserResponse] = {
    import in._
    !?(LoadPaymentAccount(externalUuid, Some(clientId))) map {
      case r: PaymentAccountLoaded if r.paymentAccount.user.isLegalUser =>
        val legalUser = r.paymentAccount.getLegalUser
        LoadLegalUserResponse(
          legalUser.legalUserType,
          legalUser.legalName,
          legalUser.siret,
          Some(legalUser.legalRepresentativeAddress),
          Some(legalUser.headQuartersAddress)
        )
      case r: PaymentAccountLoaded if r.paymentAccount.bankAccount.isDefined =>
        val bankAccount = r.paymentAccount.getBankAccount
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

}

object PaymentServer {
  def apply(sys: ActorSystem[_]): PaymentServer = {
    new PaymentServer {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit val system: ActorSystem[_] = sys
    }
  }
}
