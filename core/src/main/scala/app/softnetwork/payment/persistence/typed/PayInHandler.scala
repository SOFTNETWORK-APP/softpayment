package app.softnetwork.payment.persistence.typed

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentEvents.{
  CardRegisteredEvent,
  PaymentAccountUpsertedEvent,
  WalletRegisteredEvent
}
import app.softnetwork.payment.message.PaymentMessages.{
  PaidIn,
  PayInFailed,
  PaymentRedirection,
  PaymentRequired,
  PaymentResult
}
import app.softnetwork.payment.message.TransactionEvents.{PaidInEvent, PayInFailedEvent}
import app.softnetwork.payment.model.{PaymentAccount, Transaction}
import app.softnetwork.persistence.now
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent
import app.softnetwork.serialization.asJson
import app.softnetwork.persistence.typed._
import app.softnetwork.time._
import org.slf4j.Logger

trait PayInHandler { _: PaymentCommandHandler =>

  @InternalApi
  private[payment] def handlePayIn(
    entityId: String,
    orderUuid: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    registerCard: Boolean,
    printReceipt: Boolean,
    transaction: Transaction,
    registerWallet: Boolean = false
  )(implicit
    system: ActorSystem[_],
    log: Logger,
    softPayClientSettings: SoftPayClientSettings
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    keyValueDao.addKeyValue(
      transaction.id,
      entityId
    ) // add transaction id as a key for this payment account
    val lastUpdated = now()
    var updatedPaymentAccount =
      paymentAccount
        .withTransactions(
          paymentAccount.transactions
            .filterNot(_.id == transaction.id) :+ transaction
        )
        .withLastUpdated(lastUpdated)
    val walletEvents: List[ExternalSchedulerEvent] =
      if (registerWallet) {
        List(
          WalletRegisteredEvent.defaultInstance
            .withOrderUuid(orderUuid)
            .withExternalUuid(paymentAccount.externalUuid)
            .withLastUpdated(lastUpdated)
            .copy(
              userId = paymentAccount.userId.get,
              walletId = paymentAccount.walletId.get
            )
        )
      } else {
        List.empty
      }
    transaction.status match {
      case Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT
          if transaction.paymentClientReturnUrl.isDefined =>
        Effect
          .persist(
            PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated) +: walletEvents
          )
          .thenRun(_ =>
            PaymentRequired(
              transaction.id,
              transaction.paymentClientSecret.getOrElse(""),
              transaction.paymentClientData.getOrElse(""),
              transaction.paymentClientReturnUrl.get
            ) ~> replyTo
          )
      case Transaction.TransactionStatus.TRANSACTION_CREATED
          if transaction.redirectUrl.isDefined => // 3ds | PayPal
        Effect
          .persist(
            PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated) +: walletEvents
          )
          .thenRun(_ => PaymentRedirection(transaction.redirectUrl.get) ~> replyTo)
      case _ =>
        if (transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated) {
          log.debug("Order-{} paid in: {} -> {}", orderUuid, transaction.id, asJson(transaction))
          val clientId = paymentAccount.clientId.orElse(
            Option(softPayClientSettings.clientId)
          )
          val paymentProvider = loadPaymentProvider(clientId)
          import paymentProvider._
          val registerCardEvents: List[ExternalSchedulerEvent] =
            if (registerCard) {
              transaction.cardId match {
                case Some(cardId) =>
                  loadCard(cardId) match {
                    case Some(card) =>
                      val updatedCard = updatedPaymentAccount.maybeUser match {
                        case Some(user) =>
                          card
                            .withFirstName(user.firstName)
                            .withLastName(user.lastName)
                            .withBirthday(user.birthday)
                        case _ => card
                      }
                      updatedPaymentAccount = updatedPaymentAccount.withCards(
                        updatedPaymentAccount.cards.filterNot(_.id == updatedCard.id) :+ updatedCard
                      )
                      List(
                        CardRegisteredEvent.defaultInstance
                          .withOrderUuid(orderUuid)
                          .withExternalUuid(paymentAccount.externalUuid)
                          .withCard(updatedCard)
                          .withLastUpdated(lastUpdated)
                      )
                    case _ => List.empty
                  }
                case _ => List.empty
              }
            } else {
              List.empty
            }
          updatedPaymentAccount = transaction.preAuthorizationId match {
            case Some(preAuthorizationId) =>
              transaction.preAuthorizationDebitedAmount match {
                case Some(preAuthorizationDebitedAmount)
                    if transaction.amount < preAuthorizationDebitedAmount =>
                  // validation required
                  val updatedTransaction = updatedPaymentAccount.transactions
                    .find(_.id == preAuthorizationId)
                    .map(
                      _.copy(
                        preAuthorizationValidated =
                          Some(validatePreAuthorization(transaction.orderUuid, preAuthorizationId)),
                        clientId = clientId
                      )
                    )
                  updatedPaymentAccount.withTransactions(
                    updatedPaymentAccount.transactions.filterNot(_.id == preAuthorizationId) ++ Seq(
                      updatedTransaction
                    ).flatten
                  )
                case _ => updatedPaymentAccount
              }
            case _ => updatedPaymentAccount
          }
          Effect
            .persist(
              registerCardEvents ++
              List(
                PaidInEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withTransactionId(transaction.id)
                  .withDebitedAccount(paymentAccount.externalUuid)
                  .withDebitedAmount(transaction.amount)
                  .withLastUpdated(lastUpdated)
                  .withCardId(transaction.cardId.getOrElse(""))
                  .withPaymentType(transaction.paymentType)
                  .withPrintReceipt(printReceipt)
              ) ++
              (PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated) +: walletEvents)
            )
            .thenRun(_ => PaidIn(transaction.id, transaction.status) ~> replyTo)
        } else {
          log.error(
            "Order-{} could not be paid in: {} -> {}",
            orderUuid,
            transaction.id,
            asJson(transaction)
          )
          Effect
            .persist(
              List(
                PayInFailedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withResultMessage(transaction.resultMessage)
                  .withTransaction(transaction)
              ) ++
              (PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated) +: walletEvents)
            )
            .thenRun(_ =>
              PayInFailed(transaction.id, transaction.status, transaction.resultMessage) ~> replyTo
            )
        }
    }
  }

}
