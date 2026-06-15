package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.audit.PaymentAuditLog.audit
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.message.TransactionEvents.{
  PaidOutEvent,
  PayOutFailedEvent,
  TransactionUpdatedEvent
}
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
        // Story 13.7 — orderUuid fallback shared by every payout event + audit line (see handlePayIn).
        val effectiveCorrelationId: String = cmd.correlationId.getOrElse(orderUuid)
        def auditPayoutSucceeded(transactionId: String, status: String): Unit =
          audit.event(
            effectiveCorrelationId,
            "payout_succeeded",
            "order_uuid"     -> orderUuid,
            "transaction_id" -> transactionId,
            "amount"         -> creditedAmount,
            "fees"           -> feesAmount,
            "currency"       -> currency,
            "result"         -> status
          )
        def auditPayoutFailed(result: String, transactionId: String = ""): Unit =
          audit.event(
            effectiveCorrelationId,
            "payout_failed",
            "order_uuid"     -> orderUuid,
            "transaction_id" -> transactionId,
            "amount"         -> creditedAmount,
            "fees"           -> feesAmount,
            "currency"       -> currency,
            "result"         -> result
          )
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
                        val metadata: Map[String, String] =
                          cmd.correlationId match {
                            case Some(correlationId) =>
                              Map("correlationId" -> correlationId)
                            case None => Map.empty
                          }
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
                              .withMetadata(metadata)
                          )
                        ) match {
                          case Some(transaction) =>
                            keyValueDao.addKeyValue(transaction.id, entityId)
                            val lastUpdated = now()
                            val transactionUpdatedEvent =
                              TransactionUpdatedEvent.defaultInstance
                                .withDocument(
                                  transaction
                                    .copy(
                                      clientId = clientId,
                                      payInId = payInTransactionId,
                                      creditedUserId = Some(userId)
                                    )
                                )
                                .withLastUpdated(lastUpdated)
                                .copy(correlationId = Some(effectiveCorrelationId))
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
                                      .copy(
                                        externalReference = externalReference,
                                        correlationId = Some(effectiveCorrelationId)
                                      )
                                  ) :+ transactionUpdatedEvent
                                )
                                .thenRun { _ =>
                                  auditPayoutFailed(transaction.resultMessage, transaction.id)
                                  PayOutFailed(
                                    transaction.id,
                                    transaction.status,
                                    transaction.resultMessage
                                  ) ~> replyTo
                                }
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
                                      .copy(
                                        externalReference = externalReference,
                                        correlationId = Some(effectiveCorrelationId)
                                      )
                                  ) :+ transactionUpdatedEvent
                                )
                                .thenRun { _ =>
                                  auditPayoutSucceeded(transaction.id, transaction.status.name)
                                  PaidOut(transaction.id, transaction.status) ~> replyTo
                                }
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
                                      .copy(
                                        externalReference = externalReference,
                                        correlationId = Some(effectiveCorrelationId)
                                      )
                                  ) :+ transactionUpdatedEvent
                                )
                                .thenRun { _ =>
                                  auditPayoutFailed(transaction.resultMessage, transaction.id)
                                  PayOutFailed(
                                    transaction.id,
                                    transaction.status,
                                    transaction.resultMessage
                                  ) ~> replyTo
                                }
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
                                    .copy(
                                      externalReference = externalReference,
                                      correlationId = Some(effectiveCorrelationId) // Story 13.7
                                    )
                                )
                              )
                              .thenRun { _ =>
                                auditPayoutFailed("no transaction returned by provider")
                                PayOutFailed(
                                  "",
                                  Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                  "no transaction returned by provider"
                                ) ~> replyTo
                              }
                        }
                      case _ =>
                        Effect
                          .persist(
                            List(
                              PayOutFailedEvent.defaultInstance
                                .withOrderUuid(orderUuid)
                                .withResultMessage("no bank account")
                                .copy(
                                  externalReference = externalReference,
                                  correlationId = Some(effectiveCorrelationId)
                                )
                            )
                          )
                          .thenRun { _ =>
                            auditPayoutFailed("no bank account")
                            PayOutFailed(
                              "",
                              Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                              "no bank account"
                            ) ~> replyTo
                          }
                    }
                  case _ =>
                    Effect
                      .persist(
                        List(
                          PayOutFailedEvent.defaultInstance
                            .withOrderUuid(orderUuid)
                            .withResultMessage("no wallet id")
                            .copy(
                              externalReference = externalReference,
                              correlationId = Some(effectiveCorrelationId)
                            )
                        )
                      )
                      .thenRun { _ =>
                        auditPayoutFailed("no wallet id")
                        PayOutFailed(
                          "",
                          Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                          "no wallet id"
                        ) ~> replyTo
                      }
                }
              case _ =>
                Effect
                  .persist(
                    List(
                      PayOutFailedEvent.defaultInstance
                        .withOrderUuid(orderUuid)
                        .withResultMessage("no payment provider user id")
                        .copy(
                          externalReference = externalReference,
                          correlationId = Some(effectiveCorrelationId)
                        )
                    )
                  )
                  .thenRun { _ =>
                    auditPayoutFailed("no payment provider user id")
                    PayOutFailed(
                      "",
                      Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                      "no payment provider user id"
                    ) ~> replyTo
                  }
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadPayOutTransaction =>
        import cmd._
        // Story 13.7 — orderUuid fallback shared by every payout event + audit line (see handlePayIn).
        val effectiveCorrelationId: String = cmd.correlationId.getOrElse(orderUuid)
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
                    val transactionUpdatedEvent =
                      TransactionUpdatedEvent.defaultInstance
                        .withDocument(
                          updatedTransaction.copy(
                            clientId = clientId,
                            creditedUserId = paymentAccount.userId
                          )
                        )
                        .withLastUpdated(lastUpdated)
                        .copy(correlationId = Some(effectiveCorrelationId))
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
                              .copy(
                                externalReference = transaction.externalReference,
                                correlationId = Some(effectiveCorrelationId)
                              )
                          ) :+ transactionUpdatedEvent
                        )
                        .thenRun { _ =>
                          audit.event(
                            effectiveCorrelationId,
                            "payout_succeeded",
                            "order_uuid"     -> orderUuid,
                            "transaction_id" -> t.id,
                            "amount"         -> t.amount,
                            "fees"           -> t.fees,
                            "currency"       -> t.currency,
                            "result"         -> t.status.name
                          )
                          PayOutTransactionLoaded(
                            transaction.id,
                            transaction.status,
                            None
                          ) ~> replyTo
                        }
                    } else {
                      Effect
                        .persist(
                          List(
                            PayOutFailedEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withResultMessage(updatedTransaction.resultMessage)
                              .withTransaction(updatedTransaction)
                              .copy(
                                externalReference = transaction.externalReference,
                                correlationId = Some(effectiveCorrelationId)
                              )
                          ) :+ transactionUpdatedEvent
                        )
                        .thenRun { _ =>
                          audit.event(
                            effectiveCorrelationId,
                            "payout_failed",
                            "order_uuid"     -> orderUuid,
                            "transaction_id" -> t.id,
                            "amount"         -> t.amount,
                            "fees"           -> t.fees,
                            "currency"       -> t.currency,
                            "result"         -> updatedTransaction.resultMessage
                          )
                          PayOutTransactionLoaded(
                            transaction.id,
                            transaction.status,
                            None
                          ) ~> replyTo
                        }
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
