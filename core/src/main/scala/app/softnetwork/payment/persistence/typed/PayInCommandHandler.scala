package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig.payInStatementDescriptor
import app.softnetwork.payment.message.PaymentEvents.PaymentAccountUpsertedEvent
import app.softnetwork.payment.message.PaymentMessages.{
  LoadPayInTransaction,
  PayIn,
  PayInCallback,
  PayInCommand,
  PayInFailed,
  PayInTransactionLoaded,
  PaymentAccountNotFound,
  PaymentResult,
  TransactionNotFound
}
import app.softnetwork.payment.message.TransactionEvents.{PaidInEvent, PayInFailedEvent}
import app.softnetwork.payment.model.NaturalUser.NaturalUserType
import app.softnetwork.payment.model.{PayInTransaction, PaymentAccount, Transaction}
import app.softnetwork.persistence.now
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent
import app.softnetwork.time._
import org.slf4j.Logger

import scala.util.{Failure, Success}

trait PayInCommandHandler
    extends EntityCommandHandler[
      PayInCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]
    with PaymentCommandHandler
    with PayInHandler
    with Completion {

  override def apply(
    entityId: String,
    state: Option[PaymentAccount],
    command: PayInCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[PayInCommand]
  )(implicit
    context: ActorContext[PayInCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)
    command match {
      case cmd: PayIn =>
        import cmd._
        var registerWallet: Boolean = false
        (state match {
          case None =>
            cmd.user match {
              case Some(user) =>
                loadPaymentAccount(
                  entityId,
                  state,
                  PaymentAccount.User.NaturalUser(user),
                  clientId
                ) match {
                  case Some(paymentAccount) =>
                    val clientId = paymentAccount.clientId
                      .orElse(cmd.clientId)
                      .orElse(
                        internalClientId
                      )
                    val paymentProvider = loadPaymentProvider(clientId)
                    import paymentProvider._
                    val lastUpdated = now()
                    (paymentAccount.userId match {
                      case None =>
                        createOrUpdatePaymentAccount(
                          Some(
                            paymentAccount.withNaturalUser(
                              user.withNaturalUserType(NaturalUserType.PAYER)
                            )
                          ),
                          acceptedTermsOfPSP = false,
                          None,
                          None
                        )
                      case some => some
                    }) match {
                      case Some(userId) =>
                        keyValueDao.addKeyValue(userId, entityId)
                        (paymentAccount.walletId match {
                          case None =>
                            registerWallet = true
                            createOrUpdateWallet(Some(userId), currency, user.externalUuid, None)
                          case some => some
                        }) match {
                          case Some(walletId) =>
                            keyValueDao.addKeyValue(walletId, entityId)
                            Some(
                              paymentAccount
                                .withPaymentAccountStatus(
                                  PaymentAccount.PaymentAccountStatus.COMPTE_OK
                                )
                                .copy(user =
                                  PaymentAccount.User.NaturalUser(
                                    user
                                      .withUserId(userId)
                                      .withWalletId(walletId)
                                      .withNaturalUserType(NaturalUserType.PAYER)
                                  )
                                )
                                .withLastUpdated(lastUpdated)
                            )
                          case _ =>
                            Some(
                              paymentAccount
                                .withPaymentAccountStatus(
                                  PaymentAccount.PaymentAccountStatus.COMPTE_OK
                                )
                                .copy(user =
                                  PaymentAccount.User.NaturalUser(
                                    user
                                      .withUserId(userId)
                                      .withNaturalUserType(NaturalUserType.PAYER)
                                  )
                                )
                                .withLastUpdated(lastUpdated)
                            )
                        }
                    }
                }
              case _ =>
                None
            }
          case some => some
        }) match {
          case Some(paymentAccount) =>
            val clientId = paymentAccount.clientId
              .orElse(cmd.clientId)
              .orElse(internalClientId)
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            paymentType match {
              case Transaction.PaymentType.CARD =>
                paymentAccount.userId match {
                  case Some(userId) =>
                    val cardId =
                      registrationId match {
                        case Some(id) =>
                          createCard(id, registrationData)
                        case _ =>
                          paymentAccount.cards
                            .find(card => card.active.getOrElse(true) && !card.expired)
                            .map(_.id)
                      }
                    // load credited payment account
                    paymentDao.loadPaymentAccount(creditedAccount, clientId) complete () match {
                      case Success(s) =>
                        s match {
                          case Some(creditedPaymentAccount) =>
                            creditedPaymentAccount.walletId match {
                              case Some(creditedWalletId) =>
                                payIn(
                                  Some(
                                    PayInTransaction.defaultInstance
                                      .withAuthorId(userId)
                                      .withDebitedAmount(debitedAmount)
                                      .withCurrency(currency)
                                      .withOrderUuid(orderUuid)
                                      .withCreditedWalletId(creditedWalletId)
                                      .withCardId(cardId.orNull)
                                      .withPaymentType(paymentType)
                                      .withStatementDescriptor(
                                        statementDescriptor.getOrElse(payInStatementDescriptor)
                                      )
                                      .withRegisterCard(registerCard)
                                      .withPrintReceipt(printReceipt)
                                      .copy(
                                        ipAddress = ipAddress,
                                        browserInfo = browserInfo
                                      )
                                  )
                                ) match {
                                  case Some(transaction) =>
                                    handlePayIn(
                                      entityId,
                                      orderUuid,
                                      replyTo,
                                      paymentAccount,
                                      registerCard,
                                      printReceipt,
                                      transaction,
                                      registerWallet
                                    )
                                  case _ =>
                                    Effect.none.thenRun(_ =>
                                      PayInFailed(
                                        "",
                                        Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                        "unknown"
                                      ) ~> replyTo
                                    )
                                }
                              case _ =>
                                Effect.none.thenRun(_ =>
                                  PayInFailed(
                                    "",
                                    Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                    "no credited wallet"
                                  ) ~> replyTo
                                )
                            }
                          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                        }
                      case Failure(_) =>
                        Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                    }
                  case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                }

              case Transaction.PaymentType.PAYPAL =>
                paymentAccount.userId match {
                  case Some(userId) =>
                    // load credited payment account
                    paymentDao.loadPaymentAccount(creditedAccount, clientId) complete () match {
                      case Success(s) =>
                        s match {
                          case Some(creditedPaymentAccount) =>
                            creditedPaymentAccount.walletId match {
                              case Some(creditedWalletId) =>
                                payIn(
                                  Some(
                                    PayInTransaction.defaultInstance
                                      .withPaymentType(Transaction.PaymentType.PAYPAL)
                                      .withAuthorId(userId)
                                      .withDebitedAmount(debitedAmount)
                                      .withFeesAmount(feesAmount.getOrElse(0))
                                      .withCurrency(currency)
                                      .withOrderUuid(orderUuid)
                                      .withCreditedWalletId(creditedWalletId)
                                      .withStatementDescriptor(
                                        statementDescriptor.getOrElse(payInStatementDescriptor)
                                      )
                                      .withPrintReceipt(printReceipt)
                                      .copy(
                                        ipAddress = ipAddress,
                                        browserInfo = browserInfo
                                      )
                                  )
                                ) match {
                                  case Some(transaction) =>
                                    handlePayIn(
                                      entityId,
                                      orderUuid,
                                      replyTo,
                                      paymentAccount,
                                      registerCard = false,
                                      printReceipt = printReceipt,
                                      transaction,
                                      registerWallet
                                    )
                                  case _ =>
                                    Effect.none.thenRun(_ =>
                                      PayInFailed(
                                        "",
                                        Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                        "unknown"
                                      ) ~> replyTo
                                    )
                                }
                              case _ =>
                                Effect.none.thenRun(_ =>
                                  PayInFailed(
                                    "",
                                    Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                    "no credited wallet"
                                  ) ~> replyTo
                                )
                            }
                          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                        }
                      case Failure(_) =>
                        Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                    }
                  case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                }

              case _ =>
                Effect
                  .persist(
                    List(
                      PaidInEvent.defaultInstance
                        .withOrderUuid(orderUuid)
                        .withTransactionId("")
                        .withDebitedAccount(paymentAccount.externalUuid)
                        .withDebitedAmount(debitedAmount)
                        .withLastUpdated(now())
                        .withCardId("")
                        .withPaymentType(paymentType)
                    )
                  )
                  .thenRun(_ =>
                    PayInFailed(
                      "",
                      Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                      s"$paymentType not supported"
                    ) ~> replyTo
                  )
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PayInCallback =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            loadPayInTransaction(orderUuid, transactionId, None) match {
              case Some(transaction) =>
                handlePayIn(
                  entityId,
                  orderUuid,
                  replyTo,
                  paymentAccount,
                  registerCard = registerCard,
                  printReceipt = printReceipt,
                  transaction
                )
              case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadPayInTransaction =>
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
                  PayInTransactionLoaded(transaction.id, transaction.status, None) ~> replyTo
                )
              case Some(transaction) =>
                val clientId = paymentAccount.clientId
                  .orElse(cmd.clientId)
                  .orElse(
                    internalClientId
                  )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                loadPayInTransaction(orderUuid, transactionId, None) match {
                  case Some(t) =>
                    val lastUpdated = now()
                    val updatedTransaction = transaction
                      .withStatus(t.status)
                      .withId(t.id)
                      .withAuthorId(t.authorId)
                      .withAmount(t.amount)
                      .withLastUpdated(lastUpdated)
                      .withPaymentType(t.paymentType)
                      .withResultCode(t.resultCode)
                      .withResultMessage(t.resultMessage)
                      .copy(
                        cardId = t.cardId.orElse(transaction.cardId)
                      )
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
                            PaidInEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withTransactionId(t.id)
                              .withDebitedAccount(t.authorId)
                              .withDebitedAmount(t.amount)
                              .withLastUpdated(lastUpdated)
                              .withCardId(t.cardId.orElse(transaction.cardId).getOrElse(""))
                              .withPaymentType(t.paymentType)
                          ) :+
                          PaymentAccountUpsertedEvent.defaultInstance
                            .withLastUpdated(lastUpdated)
                            .withDocument(updatedPaymentAccount)
                        )
                        .thenRun(_ =>
                          PayInTransactionLoaded(
                            transaction.id,
                            transaction.status,
                            None
                          ) ~> replyTo
                        )
                    } else {
                      Effect
                        .persist(
                          List(
                            PayInFailedEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withResultMessage(t.resultMessage)
                              .withTransaction(updatedTransaction)
                          ) :+
                          PaymentAccountUpsertedEvent.defaultInstance
                            .withLastUpdated(lastUpdated)
                            .withDocument(updatedPaymentAccount)
                        )
                        .thenRun(_ =>
                          PayInTransactionLoaded(
                            transaction.id,
                            transaction.status,
                            None
                          ) ~> replyTo
                        )
                    }
                  case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
                }
              case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

    }
  }

}
