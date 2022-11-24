package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.grpc.GrpcClientSettings
import app.softnetwork.payment.model.{PaymentAccount, RecurringPayment}

import java.util.Date
import scala.concurrent.Future

trait PaymentClient extends GrpcClient {
  implicit lazy val akkaGrpcClient: PaymentServiceApiClient =
    PaymentServiceApiClient(
      GrpcClientSettings.fromConfig(name)
    )

  def createOrUpdatePaymentAccount(paymentAccount: PaymentAccount): Future[Boolean] = {
    akkaGrpcClient.createOrUpdatePaymentAccount(CreateOrUpdatePaymentAccountRequest(Some(paymentAccount))) map(_.succeeded)
  }

  def payInWithCardPreAuthorized(preAuthorizationId: String, creditedAccount: String): Future[TransactionResponse] = {
    akkaGrpcClient.payInWithCardPreAuthorized(PayInWithCardPreAuthorizedRequest(preAuthorizationId, creditedAccount))
  }

  def cancelPreAuthorization(orderUuid: String, cardPreAuthorizedTransactionId: String): Future[Option[Boolean]] = {
    akkaGrpcClient.cancelPreAuthorization(CancelPreAuthorizationRequest(orderUuid, cardPreAuthorizedTransactionId)) map(_.preAuthorizationCanceled)
  }

  def refund(orderUuid: String, 
             payInTransactionId: String, 
             refundAmount: Int, 
             currency: String, 
             reasonMessage: String, 
             initializedByClient: Boolean): Future[TransactionResponse] = {
    akkaGrpcClient.refund(
      RefundRequest(
        orderUuid, payInTransactionId, refundAmount, currency, reasonMessage, initializedByClient
      )
    )
  }

  def payOut(orderUuid: String, 
             creditedAccount: String, 
             creditedAmount: Int, 
             feesAmount: Int,
             currency: String): Future[TransactionResponse] = {
    akkaGrpcClient.payOut(PayOutRequest(orderUuid, creditedAccount, creditedAmount, feesAmount, currency))
  }

  def transfer(orderUuid: Option[String],
               debitedAccount: String, 
               creditedAccount: String, 
               debitedAmount: Int, 
               feesAmount: Int,
               currency: String, 
               payOutRequired: Boolean, 
               externalReference: Option[String]): Future[TransferResponse] = {
    akkaGrpcClient.transfer(
      TransferRequest(
        orderUuid,
        debitedAccount,
        creditedAccount,
        debitedAmount,
        feesAmount,
        currency,
        payOutRequired,
        externalReference
      )
    )
  }

  def directDebit(creditedAccount: String, 
                  debitedAmount: Int, 
                  feesAmount: Int,
                  currency: String, 
                  statementDescriptor: String, 
                  externalReference: Option[String]): Future[TransactionResponse] = {
    akkaGrpcClient.directDebit(
      DirectDebitRequest(
        creditedAccount, debitedAmount, feesAmount, currency, statementDescriptor, externalReference
      )
    )
  }

  def loadDirectDebitTransaction(directDebitTransactionId: String): Future[TransactionResponse] = {
    akkaGrpcClient.loadDirectDebitTransaction(LoadDirectDebitTransactionRequest(directDebitTransactionId))
  }

  def registerRecurringPayment(debitedAccount: String,
                               firstDebitedAmount: Int,
                               firstFeesAmount: Int,
                               currency: String,
                               `type`: RecurringPayment.RecurringPaymentType,
                               startDate: Option[Date],
                               endDate: Option[Date],
                               frequency: Option[RecurringPayment.RecurringPaymentFrequency],
                               fixedNextAmount: Option[Boolean],
                               nextDebitedAmount: Option[Int],
                               nextFeesAmount: Option[Int]): Future[Option[String]] = {
    akkaGrpcClient.registerRecurringPayment(
      RegisterRecurringPaymentRequest(
        debitedAccount,
        firstDebitedAmount,
        firstFeesAmount,
        currency,
        `type`,
        startDate,
        endDate,
        frequency match {
          case Some(f) => f
          case _ => RegisterRecurringPaymentRequest.RecurringPaymentFrequency.UNKNOWN_PAYMENT_FREQUENCY
        },
        fixedNextAmount,
        nextDebitedAmount,
        nextFeesAmount
      )
    ) map(_.recurringPaymentRegistrationId)
  }

  def cancelMandate(externalUuid: String): Future[Boolean] = {
    akkaGrpcClient.cancelMandate(CancelMandateRequest(externalUuid)) map(_.succeeded)
  }

}

object PaymentClient extends GrpcClientFactory[PaymentClient] {
  override val name: String = "PaymentService"
  override def init(sys: ActorSystem[_]): PaymentClient = {
    new PaymentClient {
      override implicit lazy val system: ActorSystem[_] = sys
      val name: String = PaymentClient.name
    }
  }
}
