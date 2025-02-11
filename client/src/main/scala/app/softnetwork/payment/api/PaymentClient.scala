package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.grpc.GrpcClientSettings
import app.softnetwork.api.server.client.{GrpcClient, GrpcClientFactory}
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.api.serialization._
import app.softnetwork.payment.model.{
  BankAccountOwner,
  LegalUserDetails,
  PaymentAccount,
  RecurringPayment
}
import org.softnetwork.session.model.JwtClaims

import java.util.Date
import scala.concurrent.Future

trait PaymentClient extends GrpcClient {
  implicit lazy val grpcClient: PaymentServiceApiClient =
    PaymentServiceApiClient(
      GrpcClientSettings.fromConfig(name)
    )

  lazy val settings: SoftPayClientSettings = SoftPayClientSettings(system)

  private lazy val generatedToken: String = settings.generateToken()

  def createOrUpdatePaymentAccount(
    paymentAccount: PaymentAccount,
    token: Option[String] = None
  ): Future[Boolean] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.createOrUpdatePaymentAccount(),
      t
    )
      .invoke(
        CreateOrUpdatePaymentAccountRequest(
          Some(paymentAccount.withClientId(clientId.getOrElse(settings.clientId)))
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
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.payInWithPreAuthorization(),
      t
    )
      .invoke(
        PayInWithPreAuthorizationRequest(
          preAuthorizationId,
          creditedAccount,
          debitedAmount,
          feesAmount,
          clientId.getOrElse(settings.clientId)
        )
      )
  }

  def loadPayInTransaction(
    orderUuid: String,
    payInTransactionId: String,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.loadPayInTransaction(),
      t
    )
      .invoke(
        LoadPayInTransactionRequest(
          orderUuid,
          payInTransactionId,
          clientId.getOrElse(settings.clientId)
        )
      )
  }

  def cancelPreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String,
    token: Option[String] = None
  ): Future[Option[Boolean]] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.cancelPreAuthorization(),
      t
    )
      .invoke(
        CancelPreAuthorizationRequest(
          orderUuid,
          cardPreAuthorizedTransactionId,
          clientId.getOrElse(settings.clientId)
        )
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
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.refund(),
      t
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
          clientId.getOrElse(settings.clientId)
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
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.payOut(),
      t
    )
      .invoke(
        PayOutRequest(
          orderUuid,
          creditedAccount,
          creditedAmount,
          feesAmount,
          currency,
          externalReference,
          clientId.getOrElse(settings.clientId),
          payInTransactionId
        )
      )
  }

  def loadPayOutTransaction(
    orderUuid: String,
    payOutTransactionId: String,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.loadPayOutTransaction(),
      t
    )
      .invoke(
        LoadPayOutTransactionRequest(
          orderUuid,
          payOutTransactionId,
          clientId.getOrElse(settings.clientId)
        )
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
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.transfer(),
      t
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
          clientId.getOrElse(settings.clientId)
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
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.directDebit(),
      t
    )
      .invoke(
        DirectDebitRequest(
          creditedAccount,
          debitedAmount,
          feesAmount,
          currency,
          statementDescriptor,
          externalReference,
          clientId.getOrElse(settings.clientId)
        )
      )
  }

  def loadDirectDebitTransaction(
    directDebitTransactionId: String,
    token: Option[String] = None
  ): Future[TransactionResponse] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.loadDirectDebitTransaction(),
      t
    )
      .invoke(
        LoadDirectDebitTransactionRequest(
          directDebitTransactionId,
          clientId.getOrElse(settings.clientId)
        )
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
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.registerRecurringPayment(),
      t
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
          clientId.getOrElse(settings.clientId)
        )
      ) map (_.recurringPaymentRegistrationId)
  }

  def cancelMandate(externalUuid: String, token: Option[String] = None): Future[Boolean] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.cancelMandate(),
      t
    )
      .invoke(
        CancelMandateRequest(externalUuid, clientId.getOrElse(settings.clientId))
      ) map (_.succeeded)
  }

  def loadBankAccountOwner(
    externalUuid: String,
    token: Option[String] = None
  ): Future[BankAccountOwner] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.loadBankAccountOwner(),
      t
    )
      .invoke(
        LoadBankAccountOwnerRequest(externalUuid, clientId.getOrElse(settings.clientId))
      ) map (response => BankAccountOwner(response.ownerName, response.ownerAddress))
  }

  def loadLegalUserDetails(
    externalUuid: String,
    token: Option[String] = None
  ): Future[LegalUserDetails] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.loadLegalUser(),
      t
    )
      .invoke(LoadLegalUserRequest(externalUuid, clientId.getOrElse(settings.clientId))) map (
      response =>
        LegalUserDetails(
          response.legalUserType,
          response.legalName,
          response.siret,
          response.legalRepresentativeAddress,
          response.headQuartersAddress
        )
    )
  }

  def loadBalance(
    currency: String,
    externalUuid: Option[String],
    token: Option[String] = None
  ): Future[Option[Int]] = {
    val t = token.getOrElse(generatedToken)
    val clientId = JwtClaims(t).clientId
    oauth2(
      grpcClient.loadBalance(),
      t
    )
      .invoke(
        LoadBalanceRequest(
          currency,
          externalUuid,
          clientId.getOrElse(settings.clientId)
        )
      ) map (_.balance)
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
