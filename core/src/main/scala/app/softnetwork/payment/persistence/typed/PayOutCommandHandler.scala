package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentEvents.PaymentAccountUpsertedEvent
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.message.TransactionEvents.{PaidOutEvent, PayOutFailedEvent}
import app.softnetwork.payment.model.{PayOutTransaction, PaymentAccount, Transaction}
import app.softnetwork.persistence.now
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent
import app.softnetwork.serialization.asJson
import app.softnetwork.time._
import org.slf4j.Logger

trait PayOutCommandHandler
    extends EntityCommandHandler[
      PayOutCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]
    with PaymentCommandHandler
    with Completion {

  override def apply(
    entityId: String,
    state: Option[PaymentAccount],
    command: PayOutCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[PayOutCommand]
  )(implicit
    context: ActorContext[PayOutCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)
    command match {
      case cmd: PayOut =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val clientId = paymentAccount.clientId
              .orElse(cmd.clientId)
              .orElse(
                internalClientId
              )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            paymentAccount.userId match {
              case Some(userId) =>
                paymentAccount.walletId match {
                  case Some(walletId) =>
                    paymentAccount.bankAccount.flatMap(_.id) match {
                      case Some(bankAccountId) =>
                        val pit = payInTransactionId.orElse(
                          paymentAccount.transactions
                            .filter(t =>
                              t.`type` == Transaction.TransactionType.PAYIN &&
                              (t.status.isTransactionCreated || t.status.isTransactionSucceeded)
                            )
                            .find(_.orderUuid == orderUuid)
                            .map(_.id)
                        )
                        payOut(
                          Some(
                            PayOutTransaction.defaultInstance
                              .withBankAccountId(bankAccountId)
                              .withDebitedAmount(creditedAmount)
                              .withOrderUuid(orderUuid)
                              .withFeesAmount(feesAmount)
                              .withCurrency(currency)
                              .withAuthorId(userId)
                              .withCreditedUserId(userId)
                              .withDebitedWalletId(walletId)
                              .copy(
                                externalReference = externalReference,
                                payInTransactionId = pit
                              )
                          )
                        ) match {
                          case Some(transaction) =>
                            keyValueDao.addKeyValue(transaction.id, entityId)
                            val lastUpdated = now()
                            val updatedPaymentAccount = paymentAccount
                              .withTransactions(
                                paymentAccount.transactions.filterNot(_.id == transaction.id)
                                :+ transaction.copy(clientId = clientId)
                              )
                              .withLastUpdated(lastUpdated)
                            if (transaction.status.isTransactionFailedForTechnicalReason) {
                              log.error(
                                "Order-{} could not be paid out: {} -> {}",
                                orderUuid,
                                transaction.id,
                                asJson(transaction)
                              )
                              Effect
                                .persist(
                                  List(
                                    PayOutFailedEvent.defaultInstance
                                      .withOrderUuid(orderUuid)
                                      .withResultMessage(transaction.resultMessage)
                                      .withTransaction(transaction)
                                      .copy(externalReference = externalReference)
                                  ) :+
                                  PaymentAccountUpsertedEvent.defaultInstance
                                    .withDocument(updatedPaymentAccount)
                                    .withLastUpdated(lastUpdated)
                                )
                                .thenRun(_ =>
                                  PayOutFailed(
                                    transaction.id,
                                    transaction.status,
                                    transaction.resultMessage
                                  ) ~> replyTo
                                )
                            } else if (
                              transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated
                            ) {
                              log.info(
                                "Order-{} paid out : {} -> {}",
                                orderUuid,
                                transaction.id,
                                asJson(transaction)
                              )
                              Effect
                                .persist(
                                  List(
                                    PaidOutEvent.defaultInstance
                                      .withOrderUuid(orderUuid)
                                      .withLastUpdated(lastUpdated)
                                      .withCreditedAccount(paymentAccount.externalUuid)
                                      .withCreditedAmount(creditedAmount)
                                      .withFeesAmount(feesAmount)
                                      .withCurrency(currency)
                                      .withTransactionId(transaction.id)
                                      .withPaymentType(transaction.paymentType)
                                      .copy(externalReference = externalReference)
                                  ) :+
                                  PaymentAccountUpsertedEvent.defaultInstance
                                    .withDocument(updatedPaymentAccount)
                                    .withLastUpdated(lastUpdated)
                                )
                                .thenRun(_ =>
                                  PaidOut(transaction.id, transaction.status) ~> replyTo
                                )
                            } else {
                              log.error(
                                "Order-{} could not be paid out : {} -> {}",
                                orderUuid,
                                transaction.id,
                                asJson(transaction)
                              )
                              Effect
                                .persist(
                                  List(
                                    PayOutFailedEvent.defaultInstance
                                      .withOrderUuid(orderUuid)
                                      .withResultMessage(transaction.resultMessage)
                                      .withTransaction(transaction)
                                      .copy(externalReference = externalReference)
                                  ) :+
                                  PaymentAccountUpsertedEvent.defaultInstance
                                    .withDocument(updatedPaymentAccount)
                                    .withLastUpdated(lastUpdated)
                                )
                                .thenRun(_ =>
                                  PayOutFailed(
                                    transaction.id,
                                    transaction.status,
                                    transaction.resultMessage
                                  ) ~> replyTo
                                )
                            }
                          case _ =>
                            log.error(
                              "Order-{} could not be paid out: no transaction returned by provider",
                              orderUuid
                            )
                            Effect
                              .persist(
                                List(
                                  PayOutFailedEvent.defaultInstance
                                    .withOrderUuid(orderUuid)
                                    .withResultMessage("no transaction returned by provider")
                                    .copy(externalReference = externalReference)
                                )
                              )
                              .thenRun(_ =>
                                PayOutFailed(
                                  "",
                                  Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                  "no transaction returned by provider"
                                ) ~> replyTo
                              )
                        }
                      case _ =>
                        Effect
                          .persist(
                            List(
                              PayOutFailedEvent.defaultInstance
                                .withOrderUuid(orderUuid)
                                .withResultMessage("no bank account")
                                .copy(externalReference = externalReference)
                            )
                          )
                          .thenRun(_ =>
                            PayOutFailed(
                              "",
                              Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                              "no bank account"
                            ) ~> replyTo
                          )
                    }
                  case _ =>
                    Effect
                      .persist(
                        List(
                          PayOutFailedEvent.defaultInstance
                            .withOrderUuid(orderUuid)
                            .withResultMessage("no wallet id")
                            .copy(externalReference = externalReference)
                        )
                      )
                      .thenRun(_ =>
                        PayOutFailed(
                          "",
                          Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                          "no wallet id"
                        ) ~> replyTo
                      )
                }
              case _ =>
                Effect
                  .persist(
                    List(
                      PayOutFailedEvent.defaultInstance
                        .withOrderUuid(orderUuid)
                        .withResultMessage("no payment provider user id")
                        .copy(externalReference = externalReference)
                    )
                  )
                  .thenRun(_ =>
                    PayOutFailed(
                      "",
                      Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                      "no payment provider user id"
                    ) ~> replyTo
                  )
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadPayOutTransaction =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.transactions.find(t =>
              t.id == transactionId && t.orderUuid == orderUuid
            ) match {
              case Some(transaction)
                  if transaction.status.isTransactionSucceeded
                    || transaction.status.isTransactionFailed
                    || transaction.status.isTransactionCanceled =>
                Effect.none.thenRun(_ =>
                  PayOutTransactionLoaded(transaction.id, transaction.status, None) ~> replyTo
                )
              case Some(transaction) =>
                val clientId = paymentAccount.clientId
                  .orElse(cmd.clientId)
                  .orElse(
                    internalClientId
                  )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                loadPayOutTransaction(orderUuid, transactionId) match {
                  case Some(t) =>
                    val lastUpdated = now()
                    val updatedTransaction = transaction
                      .withStatus(t.status)
                      .withLastUpdated(lastUpdated)
                      .withResultCode(t.resultCode)
                      .withResultMessage(t.resultMessage)
                    val updatedPaymentAccount = paymentAccount
                      .withTransactions(
                        paymentAccount.transactions.filterNot(_.id == t.id)
                        :+ updatedTransaction.copy(clientId = clientId)
                      )
                      .withLastUpdated(lastUpdated)
                    if (t.status.isTransactionSucceeded || t.status.isTransactionCreated) {
                      Effect
                        .persist(
                          List(
                            PaidOutEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withLastUpdated(lastUpdated)
                              .withCreditedAccount(paymentAccount.externalUuid)
                              .withCreditedAmount(t.amount)
                              .withFeesAmount(t.fees)
                              .withCurrency(t.currency)
                              .withTransactionId(t.id)
                              .withPaymentType(t.paymentType)
                              .copy(externalReference = transaction.externalReference)
                          ) :+
                          PaymentAccountUpsertedEvent.defaultInstance
                            .withLastUpdated(lastUpdated)
                            .withDocument(updatedPaymentAccount)
                        )
                        .thenRun(_ =>
                          PayOutTransactionLoaded(
                            transaction.id,
                            transaction.status,
                            None
                          ) ~> replyTo
                        )
                    } else {
                      Effect
                        .persist(
                          List(
                            PayOutFailedEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withResultMessage(updatedTransaction.resultMessage)
                              .withTransaction(updatedTransaction)
                              .copy(externalReference = transaction.externalReference)
                          ) :+
                          PaymentAccountUpsertedEvent.defaultInstance
                            .withLastUpdated(lastUpdated)
                            .withDocument(updatedPaymentAccount)
                        )
                        .thenRun(_ =>
                          PayOutTransactionLoaded(
                            transaction.id,
                            transaction.status,
                            None
                          ) ~> replyTo
                        )
                    }
                  case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

    }
  }

}
