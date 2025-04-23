package app.softnetwork.payment.persistence.typed

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentEvents.{
  PaymentAccountUpsertedEvent,
  RecurringPaymentRegisteredEvent
}
import app.softnetwork.payment.message.PaymentMessages.{
  CardNotFound,
  ExecuteFirstRecurringPayment,
  ExecuteNextRecurringPayment,
  FirstRecurringCardPaymentFailed,
  FirstRecurringPaidIn,
  FirstRecurringPaymentCallback,
  IllegalMandateStatus,
  LoadRecurringPayment,
  MandateNotFound,
  MandateRequired,
  NextRecurringPaid,
  NextRecurringPaymentFailed,
  PaymentAccountNotFound,
  PaymentRedirection,
  PaymentResult,
  RecurringCardPaymentRegistrationNotUpdated,
  RecurringCardPaymentRegistrationUpdated,
  RecurringPaymentCommand,
  RecurringPaymentLoaded,
  RecurringPaymentNotFound,
  RecurringPaymentNotRegistered,
  RecurringPaymentRegistered,
  RegisterRecurringPayment,
  UpdateRecurringCardPaymentRegistration,
  UserNotFound,
  WalletNotFound
}
import app.softnetwork.payment.message.TransactionEvents.{
  FirstRecurringCardPaymentFailedEvent,
  FirstRecurringPaidInEvent,
  NextRecurringPaidEvent,
  NextRecurringPaymentFailedEvent,
  TransactionUpdatedEvent
}
import app.softnetwork.payment.model.{
  DirectDebitTransaction,
  FirstRecurringPaymentTransaction,
  PaymentAccount,
  RecurringPayment,
  RecurringPaymentTransaction,
  Transaction
}
import app.softnetwork.persistence.{generateUUID, now}
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.{AddSchedule, RemoveSchedule}
import app.softnetwork.scheduler.message.SchedulerEvents.{
  ExternalEntityToSchedulerEvent,
  ExternalSchedulerEvent
}
import app.softnetwork.scheduler.model.Schedule
import app.softnetwork.serialization.asJson
import app.softnetwork.time._
import org.slf4j.Logger

trait RecurringPaymentCommandHandler
    extends EntityCommandHandler[
      RecurringPaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]
    with PaymentCommandHandler
    with PaymentTimers
    with Completion {

  def persistenceId: String

  override def apply(
    entityId: String,
    state: Option[PaymentAccount],
    command: RecurringPaymentCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[RecurringPaymentCommand]
  )(implicit
    context: ActorContext[RecurringPaymentCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)

    command match {
      case cmd: RegisterRecurringPayment =>
        state match {
          case Some(paymentAccount) =>
            cmd.`type` match {
              case RecurringPayment.RecurringPaymentType.CARD => // PayIns
                paymentAccount.userId match {
                  case Some(userId) =>
                    paymentAccount.walletId match {
                      case Some(walletId) =>
                        paymentAccount.cards
                          .filterNot(_.expired)
                          .find(_.getActive)
                          .map(_.id) match {
                          case Some(cardId) =>
                            val createdDate = now()
                            var recurringPayment =
                              RecurringPayment.defaultInstance
                                .withCreatedDate(createdDate)
                                .withLastUpdated(createdDate)
                                .withFirstDebitedAmount(cmd.firstDebitedAmount)
                                .withFirstFeesAmount(cmd.firstFeesAmount)
                                .withCurrency(cmd.currency)
                                .withType(cmd.`type`)
                                .withCardId(cardId)
                                .copy(
                                  startDate = cmd.startDate,
                                  endDate = cmd.endDate,
                                  frequency = cmd.frequency,
                                  fixedNextAmount = cmd.fixedNextAmount,
                                  nextDebitedAmount = cmd.nextDebitedAmount,
                                  nextFeesAmount = cmd.nextFeesAmount
                                )
                            val clientId = paymentAccount.clientId
                              .orElse(cmd.clientId)
                              .orElse(
                                internalClientId
                              )
                            val paymentProvider = loadPaymentProvider(clientId)
                            import paymentProvider._
                            registerRecurringCardPayment(
                              userId,
                              walletId,
                              cardId,
                              recurringPayment
                            ) match {
                              case Some(result) =>
                                recurringPayment = recurringPayment
                                  .withId(result.id)
                                  .withCardStatus(result.status)
                                  .copy(
                                    nextRecurringPaymentDate =
                                      recurringPayment.nextPaymentDate.map(_.toDate)
                                  )
                                keyValueDao.addKeyValue(recurringPayment.getId, entityId)
                                Effect
                                  .persist(
                                    List(
                                      RecurringPaymentRegisteredEvent.defaultInstance
                                        .withExternalUuid(paymentAccount.externalUuid)
                                        .withRecurringPayment(recurringPayment)
                                    ) :+
                                    PaymentAccountUpsertedEvent.defaultInstance
                                      .withDocument(
                                        paymentAccount
                                          .withRecurryingPayments(
                                            paymentAccount.recurryingPayments :+ recurringPayment
                                          )
                                          .withLastUpdated(createdDate)
                                      )
                                      .withLastUpdated(createdDate)
                                  )
                                  .thenRun(_ => RecurringPaymentRegistered(result.id) ~> replyTo)
                              case _ =>
                                Effect.none.thenRun(_ => RecurringPaymentNotRegistered ~> replyTo)
                            }
                          case _ => Effect.none.thenRun(_ => CardNotFound ~> replyTo)
                        }
                      case _ => Effect.none.thenRun(_ => WalletNotFound ~> replyTo)
                    }
                  case _ => Effect.none.thenRun(_ => UserNotFound ~> replyTo)
                }
              case _ => // DirectDebits
                if (!paymentAccount.mandateActivated) {
                  Effect.none.thenRun(_ => MandateRequired ~> replyTo)
                  //                  paymentAccount.userId match {
                  //                    case Some(userId) =>
                  //                      paymentAccount.bankAccount.flatMap(_.id) match {
                  //                        case Some(bankAccountId) =>
                  //                          addMandate(entityId, replyTo, cmd.debitedAccount, paymentAccount, userId, bankAccountId)
                  //                        case _ => Effect.none.thenRun(_ => BankAccountNotFound ~> replyTo)
                  //                      }
                  //                    case _ => Effect.none.thenRun(_ => UserNotFound ~> replyTo)
                  //                  }
                } else {
                  val today = now()
                  var recurringPayment =
                    RecurringPayment.defaultInstance
                      .withId(generateUUID())
                      .withCreatedDate(today)
                      .withLastUpdated(today)
                      .withFirstDebitedAmount(cmd.firstDebitedAmount)
                      .withFirstFeesAmount(cmd.firstFeesAmount)
                      .withCurrency(cmd.currency)
                      .withType(cmd.`type`)
                      .copy(
                        startDate = cmd.startDate,
                        endDate = cmd.endDate,
                        frequency = cmd.frequency,
                        fixedNextAmount = cmd.fixedNextAmount,
                        nextDebitedAmount = cmd.nextDebitedAmount,
                        nextFeesAmount = cmd.nextFeesAmount
                      )
                  import app.softnetwork.time._
                  val nextDirectDebit: List[ExternalEntityToSchedulerEvent] =
                    recurringPayment.nextPaymentDate.map(_.toDate) match {
                      case Some(value) =>
                        recurringPayment = recurringPayment.withNextRecurringPaymentDate(value)
                        List(
                          ExternalEntityToSchedulerEvent(
                            ExternalEntityToSchedulerEvent.Wrapped.AddSchedule(
                              AddSchedule(
                                Schedule(
                                  persistenceId,
                                  entityId,
                                  s"$nextRecurringPayment#${recurringPayment.getId}",
                                  1,
                                  Some(false),
                                  Some(value),
                                  None
                                )
                              )
                            )
                          )
                        )
                      case _ => List.empty
                    }
                  keyValueDao.addKeyValue(recurringPayment.getId, entityId)
                  Effect
                    .persist(
                      List(
                        RecurringPaymentRegisteredEvent.defaultInstance
                          .withExternalUuid(paymentAccount.externalUuid)
                          .withRecurringPayment(recurringPayment)
                      ) ++ nextDirectDebit :+
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(
                          paymentAccount
                            .withRecurryingPayments(
                              paymentAccount.recurryingPayments :+ recurringPayment
                            )
                            .withLastUpdated(today)
                        )
                        .withLastUpdated(today)
                    )
                    .thenRun(_ => RecurringPaymentRegistered(recurringPayment.getId) ~> replyTo)
                }
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: UpdateRecurringCardPaymentRegistration =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.recurryingPayments.find(
              _.getId == cmd.recurringPaymentRegistrationId
            ) match {
              case Some(recurringPayment) if recurringPayment.`type`.isCard =>
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                val cardId: Option[String] =
                  cmd.cardId match {
                    case Some(cardId) =>
                      paymentAccount.cards
                        .filterNot(_.expired)
                        .find(card => card.id == cardId && card.getActive) match {
                        case Some(_) => Some(cardId)
                        case _       => None
                      }
                    case _ => None
                  }
                updateRecurringCardPaymentRegistration(
                  cmd.recurringPaymentRegistrationId,
                  cardId,
                  cmd.status
                ) match {
                  case Some(result) =>
                    val lastUpdated = now()
                    val updatedPaymentAccount =
                      paymentAccount
                        .withRecurryingPayments(
                          paymentAccount.recurryingPayments
                            .filterNot(_.getId == cmd.recurringPaymentRegistrationId) :+
                          recurringPayment.withCardStatus(result.status)
                        )
                        .withLastUpdated(lastUpdated)
                    Effect
                      .persist(
                        List(
                          RecurringPaymentRegisteredEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withRecurringPayment(recurringPayment)
                        ) ++ {
                          if (result.status.isEnded) { // cancel scheduled payIn for recurring card payment
                            List(
                              ExternalEntityToSchedulerEvent(
                                ExternalEntityToSchedulerEvent.Wrapped.RemoveSchedule(
                                  RemoveSchedule(
                                    persistenceId,
                                    entityId,
                                    s"$nextRecurringPayment#${cmd.recurringPaymentRegistrationId}"
                                  )
                                )
                              )
                            )
                          } else {
                            List.empty
                          }
                        } :+
                        PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(updatedPaymentAccount)
                          .withLastUpdated(lastUpdated)
                      )
                      .thenRun(_ => RecurringCardPaymentRegistrationUpdated(result) ~> replyTo)
                  case _ =>
                    Effect.none.thenRun(_ => RecurringCardPaymentRegistrationNotUpdated ~> replyTo)
                }
              case _ =>
                Effect.none.thenRun(_ => RecurringCardPaymentRegistrationNotUpdated ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadRecurringPayment =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.recurryingPayments.find(
              _.getId == cmd.recurringPaymentRegistrationId
            ) match {
              case Some(recurringPayment) =>
                Effect.none.thenRun(_ => RecurringPaymentLoaded(recurringPayment) ~> replyTo)
              case _ => Effect.none.thenRun(_ => RecurringPaymentNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: ExecuteFirstRecurringPayment =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.recurryingPayments.find(
              _.getId == cmd.recurringPaymentRegistrationId
            ) match {
              case Some(recurringPayment) =>
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                createRecurringCardPayment(
                  RecurringPaymentTransaction.defaultInstance
                    .withExternalUuid(paymentAccount.externalUuid)
                    .withRecurringPaymentRegistrationId(cmd.recurringPaymentRegistrationId)
                    .withDebitedAmount(recurringPayment.firstDebitedAmount)
                    .withFeesAmount(recurringPayment.firstFeesAmount)
                    .withCurrency(recurringPayment.currency)
                    .withStatementDescriptor(cmd.statementDescriptor.getOrElse(""))
                    .withExtension(FirstRecurringPaymentTransaction.first)(
                      Some(
                        FirstRecurringPaymentTransaction.defaultInstance.copy(
                          ipAddress = cmd.ipAddress,
                          browserInfo = cmd.browserInfo
                        )
                      )
                    )
                ) match {
                  case Some(transaction) =>
                    handleRecurringPayment(
                      entityId,
                      replyTo,
                      paymentAccount,
                      recurringPayment,
                      transaction
                    )
                  case _ =>
                    Effect.none.thenRun(_ =>
                      FirstRecurringCardPaymentFailed(
                        "",
                        Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                        "no transaction"
                      ) ~> replyTo
                    )
                }
              case _ => Effect.none.thenRun(_ => RecurringPaymentNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: FirstRecurringPaymentCallback =>
        state match {
          case Some(paymentAccount) =>
            import cmd._
            paymentAccount.recurryingPayments.find(_.getId == recurringPayInRegistrationId) match {
              case Some(recurringPayment) =>
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                loadPayInTransaction("", transactionId, Some(recurringPayInRegistrationId)) match {
                  case Some(transaction) =>
                    handleRecurringPayment(
                      entityId,
                      replyTo,
                      paymentAccount,
                      recurringPayment,
                      transaction
                    )
                  case _ =>
                    Effect.none.thenRun(_ =>
                      FirstRecurringCardPaymentFailed(
                        "",
                        Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                        "no transaction"
                      ) ~> replyTo
                    )
                }
              case _ => Effect.none.thenRun(_ => RecurringPaymentNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: ExecuteNextRecurringPayment =>
        state match {
          case Some(paymentAccount) =>
            import cmd._
            paymentAccount.recurryingPayments.find(
              _.getId == recurringPaymentRegistrationId
            ) match {
              case Some(recurringPayment) =>
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                val debitedAmount = nextDebitedAmount.getOrElse(
                  recurringPayment.nextDebitedAmount.getOrElse(
                    recurringPayment.firstDebitedAmount
                  )
                )
                val feesAmount = nextFeesAmount.getOrElse(
                  recurringPayment.nextFeesAmount.getOrElse(
                    recurringPayment.firstFeesAmount
                  )
                )
                val currency = recurringPayment.currency
                recurringPayment.`type` match {
                  case RecurringPayment.RecurringPaymentType.CARD =>
                    recurringPayment.cardStatus match {
                      case Some(status) if status.isInProgress => // PayIn
                        createRecurringCardPayment(
                          RecurringPaymentTransaction.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withDebitedAmount(debitedAmount)
                            .withFeesAmount(feesAmount)
                            .withCurrency(currency)
                            .withStatementDescriptor(
                              statementDescriptor
                                .orElse(recurringPayment.statementDescriptor)
                                .getOrElse("")
                            ) // TODO
                        ) match {
                          case Some(transaction) =>
                            handleRecurringPayment(
                              entityId,
                              replyTo,
                              paymentAccount,
                              recurringPayment,
                              transaction
                            )
                          case _ =>
                            val reason = "no transaction"
                            handleNextRecurringPaymentFailure(
                              entityId,
                              replyTo,
                              paymentAccount,
                              recurringPayment,
                              debitedAmount,
                              feesAmount,
                              currency,
                              reason
                            )
                        }
                      case _ =>
                        val reason = "Illegal recurring payment card status"
                        handleNextRecurringPaymentFailure(
                          entityId,
                          replyTo,
                          paymentAccount,
                          recurringPayment,
                          debitedAmount,
                          feesAmount,
                          currency,
                          reason
                        )
                    }
                  case _ => // DirectDebit
                    paymentAccount.userId match {
                      case Some(creditedUserId) =>
                        paymentAccount.walletId match {
                          case Some(creditedWalletId) =>
                            paymentAccount.bankAccount.flatMap(_.mandateId) match {
                              case Some(mandateId) =>
                                if (paymentAccount.mandateActivated) {
                                  directDebit(
                                    Some(
                                      DirectDebitTransaction.defaultInstance
                                        .withAuthorId(creditedUserId)
                                        .withCreditedUserId(creditedUserId)
                                        .withCreditedWalletId(creditedWalletId)
                                        .withDebitedAmount(debitedAmount)
                                        .withFeesAmount(feesAmount)
                                        .withCurrency(currency)
                                        .withMandateId(mandateId)
                                        .withStatementDescriptor(statementDescriptor.getOrElse(""))
                                    )
                                  ) match {
                                    case Some(transaction) =>
                                      handleRecurringPayment(
                                        entityId,
                                        replyTo,
                                        paymentAccount,
                                        recurringPayment,
                                        transaction
                                      )
                                    case _ =>
                                      val reason = "no transaction"
                                      handleNextRecurringPaymentFailure(
                                        entityId,
                                        replyTo,
                                        paymentAccount,
                                        recurringPayment,
                                        debitedAmount,
                                        feesAmount,
                                        currency,
                                        reason
                                      )
                                  }
                                } else {
                                  val reason = IllegalMandateStatus.message
                                  handleNextRecurringPaymentFailure(
                                    entityId,
                                    replyTo,
                                    paymentAccount,
                                    recurringPayment,
                                    debitedAmount,
                                    feesAmount,
                                    currency,
                                    reason
                                  )
                                }
                              case _ =>
                                val reason = MandateNotFound.message
                                handleNextRecurringPaymentFailure(
                                  entityId,
                                  replyTo,
                                  paymentAccount,
                                  recurringPayment,
                                  debitedAmount,
                                  feesAmount,
                                  currency,
                                  reason
                                )
                            }
                          case _ =>
                            val reason = PaymentAccountNotFound.message
                            handleNextRecurringPaymentFailure(
                              entityId,
                              replyTo,
                              paymentAccount,
                              recurringPayment,
                              debitedAmount,
                              feesAmount,
                              currency,
                              reason
                            )
                        }
                      case _ =>
                        val reason = PaymentAccountNotFound.message
                        handleNextRecurringPaymentFailure(
                          entityId,
                          replyTo,
                          paymentAccount,
                          recurringPayment,
                          debitedAmount,
                          feesAmount,
                          currency,
                          reason
                        )
                    }
                }
              case _ => Effect.none.thenRun(_ => RecurringPaymentNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

    }
  }

  private[this] def handleRecurringPayment(
    entityId: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    recurringPayment: RecurringPayment,
    transaction: Transaction
  )(implicit
    system: ActorSystem[_],
    log: Logger
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    keyValueDao.addKeyValue(
      transaction.id,
      entityId
    ) // add transaction id as a key for this payment account
    val lastUpdated = now()
    var updatedPaymentAccount = paymentAccount.withLastUpdated(lastUpdated)
    val transactionUpdatedEvent =
      TransactionUpdatedEvent.defaultInstance
        .withDocument(
          transaction.copy(
            clientId = paymentAccount.clientId,
            debitedUserId = paymentAccount.userId
          )
        )
        .withLastUpdated(lastUpdated)
    transaction.status match {
      case Transaction.TransactionStatus.TRANSACTION_CREATED
          if transaction.redirectUrl.isDefined => // 3ds
        Effect
          .persist(transactionUpdatedEvent)
          .thenRun(_ => PaymentRedirection(transaction.redirectUrl.get) ~> replyTo)
      case _ =>
        val first =
          recurringPayment.getNumberOfRecurringPayments == 0 && recurringPayment.`type`.isCard
        if (transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated) {
          log.debug(
            "RecurringPayment-{} succeeded: {} -> {}",
            recurringPayment.getId,
            transaction.id,
            asJson(transaction)
          )
          var updatedRecurringPayment =
            recurringPayment
              .withNumberOfRecurringPayments(recurringPayment.getNumberOfRecurringPayments + 1)
              .withLastRecurringPaymentDate(transaction.lastUpdated)
              .withLastRecurringPaymentTransactionId(transaction.id)
              .withCumulatedDebitedAmount(
                recurringPayment.getCumulatedDebitedAmount + transaction.amount
              )
              .withCumulatedFeesAmount(recurringPayment.getCumulatedFeesAmount + transaction.fees)
          if (recurringPayment.`type`.isCard) {
            updatedRecurringPayment = updatedRecurringPayment.withCardStatus(
              RecurringPayment.RecurringCardPaymentStatus.IN_PROGRESS
            )
          }
          updatedRecurringPayment = updatedRecurringPayment.copy(
            nextRecurringPaymentDate = updatedRecurringPayment.nextPaymentDate.map(_.toDate)
          )
          updatedPaymentAccount = updatedPaymentAccount.withRecurryingPayments(
            updatedPaymentAccount.recurryingPayments.filterNot(
              _.getId == recurringPayment.getId
            ) :+ updatedRecurringPayment
          )
          Effect
            .persist(
              List(
                if (first) {
                  FirstRecurringPaidInEvent.defaultInstance
                    .withDebitedAccount(paymentAccount.externalUuid)
                    .withDebitedAmount(transaction.amount)
                    .withFeesAmount(transaction.fees)
                    .withCurrency(transaction.currency)
                    .withTransactionId(transaction.id)
                    .withFrequency(recurringPayment.getFrequency)
                    .withRecurringPaymentRegistrationId(recurringPayment.getId)
                    .withLastUpdated(lastUpdated)
                    .copy(nextRecurringPaymentDate =
                      updatedRecurringPayment.nextRecurringPaymentDate
                    )
                } else {
                  NextRecurringPaidEvent.defaultInstance
                    .withDebitedAccount(paymentAccount.externalUuid)
                    .withDebitedAmount(transaction.amount)
                    .withFeesAmount(transaction.fees)
                    .withCurrency(transaction.currency)
                    .withTransactionId(transaction.id)
                    .withFrequency(recurringPayment.getFrequency)
                    .withRecurringPaymentRegistrationId(recurringPayment.getId)
                    .withType(recurringPayment.`type`)
                    .withNumberOfRecurringPayments(
                      updatedRecurringPayment.getNumberOfRecurringPayments
                    )
                    .withCumulatedDebitedAmount(updatedRecurringPayment.getCumulatedDebitedAmount)
                    .withCumulatedFeesAmount(updatedRecurringPayment.getCumulatedFeesAmount)
                    .withLastUpdated(lastUpdated)
                    .copy(nextRecurringPaymentDate =
                      updatedRecurringPayment.nextRecurringPaymentDate
                    )
                }
              ) :+ {
                updatedRecurringPayment.nextRecurringPaymentDate match {
                  case Some(value) =>
                    ExternalEntityToSchedulerEvent(
                      ExternalEntityToSchedulerEvent.Wrapped.AddSchedule(
                        AddSchedule(
                          Schedule(
                            persistenceId,
                            entityId,
                            s"$nextRecurringPayment#${recurringPayment.getId}",
                            1,
                            Some(false),
                            Some(value),
                            None
                          )
                        )
                      )
                    )
                  case _ =>
                    ExternalEntityToSchedulerEvent(
                      ExternalEntityToSchedulerEvent.Wrapped.RemoveSchedule(
                        RemoveSchedule(
                          persistenceId,
                          entityId,
                          s"$nextRecurringPayment#${recurringPayment.getId}"
                        )
                      )
                    )
                }
              } :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated) :+ transactionUpdatedEvent
            )
            .thenRun(_ =>
              (if (first) FirstRecurringPaidIn(transaction.id, transaction.status)
               else NextRecurringPaid(transaction.id, transaction.status)) ~> replyTo
            )
        } else {
          log.error(
            "RecurringPayment-{} failed: {} -> {}",
            recurringPayment.getId,
            transaction.id,
            asJson(transaction)
          )
          Effect
            .persist(
              List(
                if (first) {
                  FirstRecurringCardPaymentFailedEvent.defaultInstance
                    .withDebitedAccount(paymentAccount.externalUuid)
                    .withResultMessage(transaction.getReasonMessage)
                    .withDebitedAmount(transaction.amount)
                    .withFeesAmount(transaction.fees)
                    .withCurrency(transaction.currency)
                    .withTransaction(transaction)
                    .withRecurringPaymentRegistrationId(recurringPayment.getId)
                    .withFrequency(recurringPayment.getFrequency)
                } else {
                  NextRecurringPaymentFailedEvent.defaultInstance
                    .withDebitedAccount(paymentAccount.externalUuid)
                    .withResultMessage(transaction.getReasonMessage)
                    .withDebitedAmount(transaction.amount)
                    .withFeesAmount(transaction.fees)
                    .withCurrency(transaction.currency)
                    .withTransaction(transaction)
                    .withRecurringPaymentRegistrationId(recurringPayment.getId)
                    .withType(recurringPayment.`type`)
                    .withFrequency(recurringPayment.getFrequency)
                    .withNumberOfRecurringPayments(recurringPayment.getNumberOfRecurringPayments)
                    .copy(lastRecurringPaymentDate = recurringPayment.lastRecurringPaymentDate)
                }
              ) :+ {
                recurringPayment.nextRecurringPaymentDate match {
                  case Some(value) =>
                    ExternalEntityToSchedulerEvent(
                      ExternalEntityToSchedulerEvent.Wrapped.AddSchedule(
                        AddSchedule(
                          Schedule(
                            persistenceId,
                            entityId,
                            s"$nextRecurringPayment#${recurringPayment.getId}",
                            1,
                            Some(false),
                            Some(value),
                            None
                          )
                        )
                      )
                    )
                  case _ =>
                    ExternalEntityToSchedulerEvent(
                      ExternalEntityToSchedulerEvent.Wrapped.RemoveSchedule(
                        RemoveSchedule(
                          persistenceId,
                          entityId,
                          s"$nextRecurringPayment#${recurringPayment.getId}"
                        )
                      )
                    )
                }
              } :+ transactionUpdatedEvent
            )
            .thenRun(_ =>
              (
                if (first)
                  FirstRecurringCardPaymentFailed(
                    transaction.id,
                    transaction.status,
                    transaction.getReasonMessage
                  )
                else
                  NextRecurringPaymentFailed(
                    transaction.id,
                    transaction.status,
                    transaction.getReasonMessage
                  )
              ) ~> replyTo
            )
        }
    }
  }

  private[this] def handleNextRecurringPaymentFailure(
    entityId: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    recurringPayment: RecurringPayment,
    debitedAmount: Int,
    feesAmount: Int,
    currency: String,
    reason: String
  )(implicit context: ActorContext[_]): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    Effect
      .persist(
        List(
          NextRecurringPaymentFailedEvent.defaultInstance
            .withDebitedAccount(paymentAccount.externalUuid)
            .withResultMessage(reason)
            .withDebitedAmount(debitedAmount)
            .withFeesAmount(feesAmount)
            .withCurrency(currency)
            .withRecurringPaymentRegistrationId(recurringPayment.getId)
            .withType(recurringPayment.`type`)
            .withFrequency(recurringPayment.getFrequency)
            .withNumberOfRecurringPayments(recurringPayment.getNumberOfRecurringPayments)
            .copy(lastRecurringPaymentDate = recurringPayment.lastRecurringPaymentDate)
        ) :+ {
          recurringPayment.nextRecurringPaymentDate match {
            case Some(value) =>
              ExternalEntityToSchedulerEvent(
                ExternalEntityToSchedulerEvent.Wrapped.AddSchedule(
                  AddSchedule(
                    Schedule(
                      persistenceId,
                      entityId,
                      s"$nextRecurringPayment#${recurringPayment.getId}",
                      1,
                      Some(false),
                      Some(value),
                      None
                    )
                  )
                )
              )
            case _ =>
              ExternalEntityToSchedulerEvent(
                ExternalEntityToSchedulerEvent.Wrapped.RemoveSchedule(
                  RemoveSchedule(
                    persistenceId,
                    entityId,
                    s"$nextRecurringPayment#${recurringPayment.getId}"
                  )
                )
              )
          }
        }
      )
      .thenRun(_ =>
        NextRecurringPaymentFailed(
          "",
          Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
          reason
        ) ~> replyTo
      )
  }

}
