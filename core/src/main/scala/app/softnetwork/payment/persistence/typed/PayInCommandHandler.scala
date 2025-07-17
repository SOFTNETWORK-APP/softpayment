package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig.payInStatementDescriptor
import app.softnetwork.payment.message.PaymentEvents.{
  PaymentAccountUpsertedEvent,
  PaymentMethodRegisteredEvent,
  WalletRegisteredEvent
}
import app.softnetwork.payment.message.PaymentMessages.{
  LoadPayInTransaction,
  PaidIn,
  PayIn,
  PayInCallback,
  PayInCommand,
  PayInFailed,
  PayInTransactionLoaded,
  PayInWithCardPreAuthorizedFailed,
  PayInWithPreAuthorization,
  PaymentAccountNotFound,
  PaymentRedirection,
  PaymentRequired,
  PaymentResult,
  TransactionNotFound
}
import app.softnetwork.payment.message.TransactionEvents.{
  PaidInEvent,
  PayInFailedEvent,
  TransactionUpdatedEvent
}
import app.softnetwork.payment.model.{Card, PayInTransaction, PaymentAccount, Paypal, Transaction}
import app.softnetwork.persistence.now
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent
import app.softnetwork.serialization.asJson
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
    with CustomerHandler
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
                val (pa, rw) = createOrUpdateCustomer(entityId, state, user, currency, clientId)
                registerWallet = rw
                pa
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
                          registerPaymentMethod(
                            id,
                            registrationData
                          )
                        case _ =>
                          cmd.paymentMethodId match {
                            case Some(paymentMethodId) =>
                              paymentAccount.cards
                                .find(card => card.id == paymentMethodId) match {
                                case Some(card) if card.enabled =>
                                  Some(paymentMethodId)
                                case Some(_) =>
                                  log.warn(s"Card $paymentMethodId selected is disabled")
                                  None
                                case _ =>
                                  None
                              }
                            case _ => None
                          }
                      }
                    // load credited payment account
                    paymentDao.loadPaymentAccount(creditedAccount, clientId) complete () match {
                      case Success(s) =>
                        s match {
                          case Some(creditedPaymentAccount) =>
                            creditedPaymentAccount.walletId match {
                              case Some(creditedWalletId) =>
                                val registerMeansOfPayment =
                                  cmd.registerMeansOfPayment.getOrElse(cmd.registerCard)
                                payIn(
                                  Some(
                                    PayInTransaction.defaultInstance
                                      .withAuthorId(userId)
                                      .withDebitedAmount(debitedAmount)
                                      .withFeesAmount(feesAmount.getOrElse(0))
                                      .withCurrency(currency)
                                      .withOrderUuid(orderUuid)
                                      .withCreditedWalletId(creditedWalletId)
                                      .withPaymentMethodId(cardId.orNull)
                                      .withPaymentType(paymentType)
                                      .withStatementDescriptor(
                                        statementDescriptor.getOrElse(payInStatementDescriptor)
                                      )
                                      .withRegisterMeansOfPayment(registerMeansOfPayment)
                                      .withPrintReceipt(printReceipt)
                                      .copy(
                                        ipAddress = ipAddress,
                                        browserInfo = browserInfo,
                                        preRegistrationId = registrationId
                                      )
                                  )
                                ) match {
                                  case Some(transaction) =>
                                    handlePayIn(
                                      entityId,
                                      orderUuid,
                                      replyTo,
                                      paymentAccount,
                                      registerMeansOfPayment,
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
                    val paypalId =
                      registrationId match {
                        case Some(id) =>
                          registerPaymentMethod(
                            id,
                            registrationData
                          )
                        case _ =>
                          cmd.paymentMethodId match {
                            case Some(paymentMethodId) =>
                              paymentAccount.paypals
                                .find(paypal => paypal.id == paymentMethodId) match {
                                case Some(paypal) if paypal.enabled =>
                                  Some(paymentMethodId)
                                case Some(_) =>
                                  log.warn(s"Paypal $paymentMethodId selected is disabled")
                                  None
                                case _ =>
                                  None
                              }
                            case _ => None
                          }
                      }
                    // load credited payment account
                    paymentDao.loadPaymentAccount(creditedAccount, clientId) complete () match {
                      case Success(s) =>
                        s match {
                          case Some(creditedPaymentAccount) =>
                            creditedPaymentAccount.walletId match {
                              case Some(creditedWalletId) =>
                                val registerMeansOfPayment =
                                  cmd.registerMeansOfPayment.getOrElse(false)
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
                                      .withPaymentMethodId(paypalId.orNull)
                                      .withStatementDescriptor(
                                        statementDescriptor.getOrElse(payInStatementDescriptor)
                                      )
                                      .withRegisterMeansOfPayment(registerMeansOfPayment)
                                      .withPrintReceipt(printReceipt)
                                      .copy(
                                        ipAddress = ipAddress,
                                        browserInfo = browserInfo,
                                        preRegistrationId = registrationId
                                      )
                                  )
                                ) match {
                                  case Some(transaction) =>
                                    handlePayIn(
                                      entityId,
                                      orderUuid,
                                      replyTo,
                                      paymentAccount,
                                      registerMeansOfPayment,
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
                        .withPaymentMethodId("")
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
                  registerMeansOfPayment = cmd.registerMeansOfPayment,
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
                    val paymentMethodId = t.paymentMethodId.orElse(transaction.paymentMethodId)
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
                        paymentMethodId = paymentMethodId
                      )
                    val transactionUpdatedEvent =
                      TransactionUpdatedEvent.defaultInstance
                        .withDocument(
                          transaction.copy(
                            clientId = clientId,
                            debitedUserId = paymentAccount.userId
                          )
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
                              .withPaymentMethodId(paymentMethodId.getOrElse(""))
                              .withPaymentType(t.paymentType)
                          ) :+ transactionUpdatedEvent
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
                          ) :+ transactionUpdatedEvent
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

      case cmd: PayInWithPreAuthorization =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val clientId = paymentAccount.clientId
              .orElse(cmd.clientId)
              .orElse(
                internalClientId
              )
            log.info(s"pay in with pre authorization: $entityId -> ${asJson(cmd)}")
            val maybeTransaction = paymentAccount.transactions
              .filter(t =>
                t.`type` == Transaction.TransactionType.PRE_AUTHORIZATION
              ) //TODO check it
              .find(_.id == preAuthorizationId)
            maybeTransaction match {
              case None =>
                handlePayInWithPreAuthorizationFailure(
                  "",
                  replyTo,
                  "PreAuthorizationTransactionNotFound"
                )
              case Some(preAuthorizationTransaction)
                  if !Seq(
                    Transaction.TransactionStatus.TRANSACTION_CREATED,
                    Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                  ).contains(
                    preAuthorizationTransaction.status
                  ) =>
                handlePayInWithPreAuthorizationFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "IllegalPreAuthorizationTransactionStatus"
                )
              case Some(preAuthorizationTransaction)
                  if preAuthorizationTransaction.preAuthorizationCanceled.getOrElse(false) =>
                handlePayInWithPreAuthorizationFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "PreAuthorizationCanceled"
                )
              case Some(preAuthorizationTransaction)
                  if preAuthorizationTransaction.preAuthorizationValidated.getOrElse(false) =>
                handlePayInWithPreAuthorizationFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "PreAuthorizationValidated"
                )
              case Some(preAuthorizationTransaction)
                  if preAuthorizationTransaction.preAuthorizationExpired.getOrElse(false) =>
                handlePayInWithPreAuthorizationFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "PreAuthorizationExpired"
                )
              case Some(preAuthorizationTransaction)
                  if debitedAmount.getOrElse(
                    preAuthorizationTransaction.amount
                  ) > preAuthorizationTransaction.amount =>
                handlePayInWithPreAuthorizationFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "DebitedAmountAbovePreAuthorizationAmount"
                )
              case Some(preAuthorizationTransaction) =>
                // load credited payment account
                paymentDao.loadPaymentAccount(creditedAccount, clientId) complete () match {
                  case Success(s) =>
                    s match {
                      case Some(creditedPaymentAccount) =>
                        val clientId = creditedPaymentAccount.clientId.orElse(
                          internalClientId
                        )
                        val paymentProvider = loadPaymentProvider(clientId)
                        import paymentProvider._
                        creditedPaymentAccount.walletId match {
                          case Some(creditedWalletId) =>
                            payIn(
                              Some(
                                PayInTransaction.defaultInstance
                                  .withPaymentType(Transaction.PaymentType.PREAUTHORIZED)
                                  .withPreAuthorizedTransactionId(preAuthorizationId)
                                  .withAuthorId(preAuthorizationTransaction.authorId)
                                  .withDebitedAmount(
                                    debitedAmount.getOrElse(preAuthorizationTransaction.amount)
                                  )
                                  .withCurrency(preAuthorizationTransaction.currency)
                                  .withOrderUuid(preAuthorizationTransaction.orderUuid)
                                  .withCreditedWalletId(creditedWalletId)
                                  .withPreAuthorizationDebitedAmount(
                                    preAuthorizationTransaction.amount
                                  )
                                  .copy(
                                    feesAmount = feesAmount.getOrElse(0),
                                    preRegistrationId =
                                      preAuthorizationTransaction.preRegistrationId
                                  )
                              )
                            ) match {
                              case Some(transaction) =>
                                handlePayIn(
                                  entityId,
                                  transaction.orderUuid,
                                  replyTo,
                                  paymentAccount,
                                  registerMeansOfPayment = false,
                                  printReceipt = false,
                                  transaction
                                )
                              case _ =>
                                handlePayInWithPreAuthorizationFailure(
                                  preAuthorizationTransaction.orderUuid,
                                  replyTo,
                                  "TransactionNotSpecified"
                                )
                            }
                          case _ =>
                            handlePayInWithPreAuthorizationFailure(
                              preAuthorizationTransaction.orderUuid,
                              replyTo,
                              "CreditedWalletNotFound"
                            )
                        }
                      case _ =>
                        handlePayInWithPreAuthorizationFailure(
                          preAuthorizationTransaction.orderUuid,
                          replyTo,
                          "CreditedPaymentAccountNotFound"
                        )
                    }
                  case Failure(_) =>
                    handlePayInWithPreAuthorizationFailure(
                      preAuthorizationTransaction.orderUuid,
                      replyTo,
                      "CreditedPaymentAccountNotFound"
                    )
                }
            }
          case _ =>
            handlePayInWithPreAuthorizationFailure(
              "",
              replyTo,
              "PaymentAccountNotFound"
            )
        }

    }
  }

  @InternalApi
  private[payment] def handlePayIn(
    entityId: String,
    orderUuid: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    registerMeansOfPayment: Boolean,
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
    var updatedPaymentAccount = paymentAccount.withLastUpdated(lastUpdated)
    var transactionUpdatedEvents = {
      List(
        TransactionUpdatedEvent.defaultInstance
          .withDocument(
            transaction
              .copy(clientId = paymentAccount.clientId, debitedUserId = paymentAccount.userId)
          )
          .withLastUpdated(lastUpdated)
      )
    }
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
            (PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated) +: walletEvents) ++ transactionUpdatedEvents
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
            (PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated) +: walletEvents) ++ transactionUpdatedEvents
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
          val registerPaymentMethodEvents: List[ExternalSchedulerEvent] =
            if (registerMeansOfPayment) {
              transaction.paymentMethodId match {
                case Some(paymentMethodId) =>
                  loadPaymentMethod(paymentMethodId) match {
                    case Some(paymentMethod) =>
                      paymentMethod match {
                        case card: Card =>
                          val updatedCard = updatedPaymentAccount.maybeUser match {
                            case Some(user) =>
                              card
                                .withFirstName(user.firstName)
                                .withLastName(user.lastName)
                                .withBirthday(user.birthday)
                            case _ => card
                          }
                          updatedPaymentAccount = updatedPaymentAccount.withCards(
                            updatedPaymentAccount.cards.filterNot(
                              _.id == updatedCard.id
                            ) :+ updatedCard
                          )
                          List(
                            PaymentMethodRegisteredEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withExternalUuid(paymentAccount.externalUuid)
                              .withCard(updatedCard)
                              .withLastUpdated(lastUpdated)
                          )
                        case paypal: Paypal =>
                          updatedPaymentAccount = updatedPaymentAccount.withPaypals(
                            updatedPaymentAccount.paypals.filterNot(_.id == paypal.id) :+ paypal
                          )
                          List(
                            PaymentMethodRegisteredEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withExternalUuid(paymentAccount.externalUuid)
                              .withPaypal(paypal)
                              .withLastUpdated(lastUpdated)
                          )
                        case _ => List.empty
                      }
                    case _ => List.empty
                  }
                case _ => List.empty
              }
            } else {
              List.empty
            }
          transaction.preAuthorizationId match {
            case Some(preAuthorizationId) =>
              transaction.preAuthorizationDebitedAmount match {
                case Some(preAuthorizationDebitedAmount)
                    if transaction.amount < preAuthorizationDebitedAmount =>
                  // validation required
                  val updatedTransaction = updatedPaymentAccount.transactions
                    .find(_.id == preAuthorizationId)
                    .map(
                      _.copy(
                        preAuthorizationValidated = Some(
                          validatePreAuthorization(
                            transaction.orderUuid,
                            preAuthorizationId
                          )
                        ),
                        clientId = clientId
                      )
                    )
                  updatedTransaction match {
                    case Some(transaction) =>
                      transactionUpdatedEvents =
                        transactionUpdatedEvents :+ TransactionUpdatedEvent.defaultInstance
                          .withDocument(
                            transaction.copy(
                              clientId = paymentAccount.clientId,
                              debitedUserId = paymentAccount.userId
                            )
                          )
                          .withLastUpdated(lastUpdated)
                    case _ =>
                  }
                case _ =>
              }
            case _ =>
          }
          Effect
            .persist(
              registerPaymentMethodEvents ++
              List(
                PaidInEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withTransactionId(transaction.id)
                  .withDebitedAccount(paymentAccount.externalUuid)
                  .withDebitedAmount(transaction.amount)
                  .withLastUpdated(lastUpdated)
                  .withPaymentMethodId(transaction.paymentMethodId.getOrElse(""))
                  .withPaymentType(transaction.paymentType)
                  .withPrintReceipt(printReceipt)
              ) ++
              (PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated) +: walletEvents) ++ transactionUpdatedEvents
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
                .withLastUpdated(lastUpdated) +: walletEvents) ++ transactionUpdatedEvents
            )
            .thenRun(_ =>
              PayInFailed(transaction.id, transaction.status, transaction.resultMessage) ~> replyTo
            )
        }
    }
  }

  private[payment] def handlePayInWithPreAuthorizationFailure(
    orderUuid: String,
    replyTo: Option[ActorRef[PaymentResult]],
    reason: String
  )(implicit context: ActorContext[_]): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {

    Effect
      .persist(
        List(
          PayInFailedEvent.defaultInstance.withOrderUuid(orderUuid).withResultMessage(reason)
        )
      )
      .thenRun(_ => PayInWithCardPreAuthorizedFailed(reason) ~> replyTo)
  }

}
