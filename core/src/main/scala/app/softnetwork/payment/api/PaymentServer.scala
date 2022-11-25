package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  CancelMandate,
  CancelPreAuthorization,
  CreateOrUpdatePaymentAccount,
  DirectDebit,
  DirectDebited,
  LoadDirectDebitTransaction,
  MandateCanceled,
  PaidIn,
  PaidOut,
  PayInWithCardPreAuthorized,
  PayOut,
  PaymentAccountCreated,
  PaymentAccountUpdated,
  PaymentCommand,
  PaymentError,
  PreAuthorizationCanceled,
  RecurringPaymentRegistered,
  Refund,
  Refunded,
  RegisterRecurringPayment,
  Transfer,
  Transfered
}
import app.softnetwork.payment.model.RecurringPayment
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
      case r: PaidIn       => TransactionResponse(transactionId = Some(r.transactionId))
      case e: PaymentError => TransactionResponse(error = Some(e.message))
      case _               => TransactionResponse(error = Some("unknown"))
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
      case r: Refunded     => TransactionResponse(transactionId = Some(r.transactionId))
      case e: PaymentError => TransactionResponse(error = Some(e.message))
      case _               => TransactionResponse(error = Some("unknown"))
    }
  }

  override def payOut(in: PayOutRequest): Future[TransactionResponse] = {
    import in._
    !?(PayOut(orderUuid, creditedAccount, creditedAmount, feesAmount, currency)) map {
      case r: PaidOut      => TransactionResponse(transactionId = Some(r.transactionId))
      case e: PaymentError => TransactionResponse(error = Some(e.message))
      case _               => TransactionResponse(error = Some("unknown"))
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
      case r: Transfered =>
        TransferResponse(Some(r.transferedTransactionId), r.paidOutTransactionId)
      case _ => TransferResponse()
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
      case r: DirectDebited => TransactionResponse(transactionId = Some(r.transactionId))
      case e: PaymentError  => TransactionResponse(error = Some(e.message))
      case _                => TransactionResponse(error = Some("unknown"))
    }
  }

  override def loadDirectDebitTransaction(
    in: LoadDirectDebitTransactionRequest
  ): Future[TransactionResponse] = {
    import in._
    !?(LoadDirectDebitTransaction(directDebitTransactionId)) map {
      case r: DirectDebited => TransactionResponse(transactionId = Some(r.transactionId))
      case e: PaymentError  => TransactionResponse(error = Some(e.message))
      case _                => TransactionResponse(error = Some("unknown"))
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
}
