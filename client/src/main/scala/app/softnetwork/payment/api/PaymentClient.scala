package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.grpc.GrpcClientSettings
import akka.grpc.scaladsl.SingleResponseRequestBuilder
import app.softnetwork.api.server.client.{GrpcClient, GrpcClientFactory}
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.api.serialization._
import app.softnetwork.payment.model.{
  BankAccountOwner,
  LegalUserDetails,
  PaymentAccount,
  RecurringPayment
}

import java.util.Date
import scala.concurrent.Future

trait PaymentClient extends GrpcClient {
  implicit lazy val grpcClient: PaymentServiceApiClient =
    PaymentServiceApiClient(
      GrpcClientSettings.fromConfig(name)
    )

  lazy val settings: SoftPayClientSettings = SoftPayClientSettings(system)

  private lazy val generatedToken: String = settings.generateToken()

  private def withAuthorization[Req, Res](
    single: SingleResponseRequestBuilder[Req, Res],
    token: Option[String]
  ): SingleResponseRequestBuilder[Req, Res] = {
    oauth2(single, token.getOrElse(generatedToken))
  }

  def createOrUpdatePaymentAccount(
    paymentAccount: PaymentAccount,
    token: Option[String] = None
  ): Future[Boolean] = {
    withAuthorization(
      grpcClient.createOrUpdatePaymentAccount(),
      token
    )
      .invoke(
        CreateOrUpdatePaymentAccountRequest(
          Some(paymentAccount.withClientId(settings.clientId))
        )
      ) map (_.succeeded)
  }

  def payInWithPreAuthorization(
    preAuthorizationId: String,
    creditedAccount: String,
    debitedAmount: Option[Int],
    feesAmount: Option[Int] = None,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.payInWithPreAuthorization(),
      token
    )
      .invoke(
        PayInWithPreAuthorizationRequest(
          preAuthorizationId,
          creditedAccount,
          debitedAmount,
          feesAmount,
          settings.clientId
        )
      )
  }

  def loadPayInTransaction(
    orderUuid: String,
    payInTransactionId: String,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.loadPayInTransaction(),
      token
    )
      .invoke(
        LoadPayInTransactionRequest(orderUuid, payInTransactionId, settings.clientId)
      )
  }

  def cancelPreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String,
    token: Option[String] = None
  ): Future[Option[Boolean]] = {
    withAuthorization(
      grpcClient.cancelPreAuthorization(),
      token
    )
      .invoke(
        CancelPreAuthorizationRequest(orderUuid, cardPreAuthorizedTransactionId, settings.clientId)
      ) map (_.preAuthorizationCanceled)
  }

  def refund(
    orderUuid: String,
    payInTransactionId: String,
    refundAmount: Int,
    feesRefundAmount: Option[Int] = None,
    currency: String,
    reasonMessage: String,
    initializedByClient: Boolean,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.refund(),
      token
    )
      .invoke(
        RefundRequest(
          orderUuid,
          payInTransactionId,
          refundAmount,
          feesRefundAmount,
          currency,
          reasonMessage,
          initializedByClient,
          settings.clientId
        )
      )
  }

  def payOut(
    orderUuid: String,
    creditedAccount: String,
    creditedAmount: Int,
    feesAmount: Int,
    currency: String,
    externalReference: Option[String],
    payInTransactionId: Option[String],
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.payOut(),
      token
    )
      .invoke(
        PayOutRequest(
          orderUuid,
          creditedAccount,
          creditedAmount,
          feesAmount,
          currency,
          externalReference,
          settings.clientId,
          payInTransactionId
        )
      )
  }

  def loadPayOutTransaction(
    orderUuid: String,
    payOutTransactionId: String,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.loadPayOutTransaction(),
      token
    )
      .invoke(
        LoadPayOutTransactionRequest(orderUuid, payOutTransactionId, settings.clientId)
      )
  }

  def transfer(
    orderUuid: Option[String],
    debitedAccount: String,
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int,
    currency: String,
    payOutRequired: Boolean,
    externalReference: Option[String],
    token: Option[String] = None
  ): Future[TransferResponse] = {
    withAuthorization(
      grpcClient.transfer(),
      token
    )
      .invoke(
        TransferRequest(
          orderUuid,
          debitedAccount,
          creditedAccount,
          debitedAmount,
          feesAmount,
          currency,
          payOutRequired,
          externalReference,
          settings.clientId
        )
      )
  }

  def directDebit(
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int,
    currency: String,
    statementDescriptor: String,
    externalReference: Option[String],
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.directDebit(),
      token
    )
      .invoke(
        DirectDebitRequest(
          creditedAccount,
          debitedAmount,
          feesAmount,
          currency,
          statementDescriptor,
          externalReference,
          settings.clientId
        )
      )
  }

  def loadDirectDebitTransaction(
    directDebitTransactionId: String,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    withAuthorization(
      grpcClient.loadDirectDebitTransaction(),
      token
    )
      .invoke(
        LoadDirectDebitTransactionRequest(directDebitTransactionId, settings.clientId)
      )
  }

  def registerRecurringPayment(
    debitedAccount: String,
    firstDebitedAmount: Int,
    firstFeesAmount: Int,
    currency: String,
    `type`: RecurringPayment.RecurringPaymentType,
    startDate: Option[Date],
    endDate: Option[Date],
    frequency: Option[RecurringPayment.RecurringPaymentFrequency],
    fixedNextAmount: Option[Boolean],
    nextDebitedAmount: Option[Int],
    nextFeesAmount: Option[Int],
    statementDescriptor: Option[String],
    externalReference: Option[String],
    token: Option[String] = None
  ): Future[Option[String]] = {
    withAuthorization(
      grpcClient.registerRecurringPayment(),
      token
    )
      .invoke(
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
            case _ =>
              RegisterRecurringPaymentRequest.RecurringPaymentFrequency.UNKNOWN_PAYMENT_FREQUENCY
          },
          fixedNextAmount,
          nextDebitedAmount,
          nextFeesAmount,
          statementDescriptor,
          externalReference,
          settings.clientId
        )
      ) map (_.recurringPaymentRegistrationId)
  }

  def cancelMandate(externalUuid: String, token: Option[String] = None): Future[Boolean] = {
    withAuthorization(
      grpcClient.cancelMandate(),
      token
    )
      .invoke(CancelMandateRequest(externalUuid)) map (_.succeeded)
  }

  def loadBankAccountOwner(
    externalUuid: String,
    token: Option[String] = None
  ): Future[BankAccountOwner] = {
    withAuthorization(
      grpcClient.loadBankAccountOwner(),
      token
    )
      .invoke(LoadBankAccountOwnerRequest(externalUuid, settings.clientId)) map (response =>
      BankAccountOwner(response.ownerName, response.ownerAddress)
    )
  }

  def loadLegalUserDetails(
    externalUuid: String,
    token: Option[String] = None
  ): Future[LegalUserDetails] = {
    withAuthorization(
      grpcClient.loadLegalUser(),
      token
    )
      .invoke(LoadLegalUserRequest(externalUuid, settings.clientId)) map (response =>
      LegalUserDetails(
        response.legalUserType,
        response.legalName,
        response.siret,
        response.legalRepresentativeAddress,
        response.headQuartersAddress
      )
    )
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
