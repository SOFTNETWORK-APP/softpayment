package app.softnetwork.payment.persistence.typed

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.kv.handlers.GenericKeyValueDao
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.config.PaymentSettings.{AkkaNodeRole, PayInStatementDescriptor}
import app.softnetwork.payment.handlers.{GenericPaymentDao, PaymentKvDao}
import app.softnetwork.payment.message.PaymentEvents._
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.message.TransactionEvents._
import app.softnetwork.payment.model.LegalUser.LegalUserType
import app.softnetwork.payment.model.PaymentUser.PaymentUserType
import app.softnetwork.payment.model._
import app.softnetwork.payment.spi._
import app.softnetwork.persistence._
import app.softnetwork.persistence.message.{BroadcastEvent, CrudEvent}
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.serialization.asJson
import app.softnetwork.time._
import org.slf4j.Logger
import app.softnetwork.scheduler.message.SchedulerEvents.{
  ExternalEntityToSchedulerEvent,
  ExternalSchedulerEvent,
  SchedulerEventWithCommand
}
import app.softnetwork.scheduler.message.{AddSchedule, RemoveSchedule}
import app.softnetwork.scheduler.model.Schedule

import java.time.LocalDate
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/** Created by smanciot on 22/04/2022.
  */
trait GenericPaymentBehavior
    extends TimeStampedBehavior[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]
    with ManifestWrapper[PaymentAccount] { _: PaymentProvider =>

  override protected val manifestWrapper: ManifestW = ManifestW()

  lazy val keyValueDao: GenericKeyValueDao =
    PaymentKvDao //FIXME app.softnetwork.payment.persistence.data.paymentKvDao

  val nextRecurringPayment: String = "NextRecurringPayment"

  def paymentDao: GenericPaymentDao

  /** @return
    *   node role required to start this actor
    */
  override lazy val role: String = AkkaNodeRole

  override def init(system: ActorSystem[_], maybeRole: Option[String] = None)(implicit
    c: ClassTag[PaymentCommand]
  ): Unit = {
    PaymentKvBehavior.init(system, maybeRole)
    super.init(system, maybeRole)
  }

  /** Set event tags, which will be used in persistence query
    *
    * @param entityId
    *   - entity id
    * @param event
    *   - the event to tag
    * @return
    *   event tags
    */
  override protected def tagEvent(entityId: String, event: ExternalSchedulerEvent): Set[String] =
    event match {
      case _: BroadcastEvent => Set(s"${persistenceId.toLowerCase}-to-external")
      case _: CrudEvent      => Set(s"${persistenceId.toLowerCase}-to-elastic")
      case _: SchedulerEventWithCommand =>
        Set(SchedulerSettings.SchedulerConfig.eventStreams.entityToSchedulerTag)
      case _ => Set(persistenceId)
    }

  /** @param entityId
    *   - entity identity
    * @param state
    *   - current state
    * @param command
    *   - command to handle
    * @param replyTo
    *   - optional actor to reply to
    * @return
    *   effect
    */
  override def handleCommand(
    entityId: String,
    state: Option[PaymentAccount],
    command: PaymentCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[PaymentCommand]
  )(implicit
    context: ActorContext[PaymentCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    command match {

      case cmd: CreateOrUpdatePaymentAccount =>
        import cmd._
        val lastUpdated = now()
        var updated = false
        var updatedPaymentAccount =
          state match {
            case Some(previousPaymentAccount) =>
              updated = true
              paymentAccount
                .withTransactions(
                  previousPaymentAccount.transactions.filterNot(transaction =>
                    paymentAccount.transactions.map(_.id).contains(transaction.id)
                  ) ++ paymentAccount.transactions
                )
                .withLastUpdated(lastUpdated)
            case _ =>
              paymentAccount.withCreatedDate(lastUpdated).withLastUpdated(lastUpdated)
          }
        keyValueDao.addKeyValue(updatedPaymentAccount.externalUuidWithProfile, entityId)
        updatedPaymentAccount.userId match {
          case Some(userId) => keyValueDao.addKeyValue(userId, entityId)
          case _            =>
        }
        updatedPaymentAccount.walletId match {
          case Some(walletId) => keyValueDao.addKeyValue(walletId, entityId)
          case _              =>
        }
        updatedPaymentAccount.bankAccount match {
          case Some(bankAccount) =>
            bankAccount.id match {
              case Some(bankAccountId) => keyValueDao.addKeyValue(bankAccountId, entityId)
              case _                   =>
            }
            bankAccount.mandateId match {
              case Some(mandateId) => keyValueDao.addKeyValue(mandateId, entityId)
              case _               =>
            }
            if (bankAccount.externalUuid.trim.isEmpty) {
              updatedPaymentAccount = updatedPaymentAccount
                .withBankAccount(
                  bankAccount.withExternalUuid(updatedPaymentAccount.externalUuid)
                )
            }
          case None =>
        }
        updatedPaymentAccount.documents.foreach { document =>
          document.id match {
            case Some(documentId) => keyValueDao.addKeyValue(documentId, entityId)
            case _                =>
          }
        }
        updatedPaymentAccount.transactions.foreach { transaction =>
          keyValueDao.addKeyValue(transaction.id, entityId)
        }
        if (updatedPaymentAccount.legalUser) {
          updatedPaymentAccount.getLegalUser.uboDeclaration match {
            case Some(declaration) => keyValueDao.addKeyValue(declaration.id, entityId)
            case _                 =>
          }
        }
        Effect
          .persist(
            broadcastEvent(
              PaymentAccountCreatedOrUpdatedEvent.defaultInstance
                .withLastUpdated(lastUpdated)
                .withExternalUuid(updatedPaymentAccount.externalUuid)
                .copy(profile = updatedPaymentAccount.profile)
            ) :+
            PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated)
          )
          .thenRun(_ => {
            val result =
              if (updated)
                PaymentAccountUpdated
              else
                PaymentAccountCreated
            result ~> replyTo
          })

      case cmd: PreRegisterCard =>
        import cmd._
        var registerWallet: Boolean = false
        loadPaymentAccount(entityId, state, PaymentAccount.User.NaturalUser(user)) match {
          case Some(paymentAccount) =>
            val lastUpdated = now()
            (paymentAccount.userId match {
              case None =>
                createOrUpdatePaymentAccount(
                  Some(
                    paymentAccount.withNaturalUser(
                      user.withPaymentUserType(PaymentUserType.PAYER)
                    )
                  )
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
                    val createOrUpdatePaymentAccount =
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(
                          paymentAccount
                            .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
                            .copy(user =
                              PaymentAccount.User.NaturalUser(
                                user
                                  .withUserId(userId)
                                  .withWalletId(walletId)
                                  .withPaymentUserType(PaymentUserType.PAYER)
                              )
                            )
                            .withLastUpdated(lastUpdated)
                        )
                        .withLastUpdated(lastUpdated)
                    preRegisterCard(Some(userId), currency, user.externalUuid) match {
                      case Some(cardPreRegistration) =>
                        keyValueDao.addKeyValue(cardPreRegistration.id, entityId)
                        val walletEvents: List[ExternalSchedulerEvent] =
                          if (registerWallet) {
                            broadcastEvent(
                              WalletRegisteredEvent.defaultInstance
                                .withOrderUuid(orderUuid)
                                .withExternalUuid(user.externalUuid)
                                .withUserId(userId)
                                .withWalletId(walletId)
                                .withLastUpdated(lastUpdated)
                            )
                          } else {
                            List.empty
                          }
                        Effect
                          .persist(
                            broadcastEvent(
                              CardPreRegisteredEvent.defaultInstance
                                .withOrderUuid(orderUuid)
                                .withLastUpdated(lastUpdated)
                                .withExternalUuid(user.externalUuid)
                                .withUserId(userId)
                                .withWalletId(walletId)
                                .withCardPreRegistrationId(cardPreRegistration.id)
                                .withOwner(
                                  CardOwner.defaultInstance
                                    .withFirstName(user.firstName)
                                    .withLastName(user.lastName)
                                    .withBirthday(user.birthday)
                                )
                            ) ++ walletEvents :+ createOrUpdatePaymentAccount
                          )
                          .thenRun(_ => CardPreRegistered(cardPreRegistration) ~> replyTo)
                      case _ =>
                        if (registerWallet) {
                          Effect
                            .persist(
                              broadcastEvent(
                                WalletRegisteredEvent.defaultInstance
                                  .withOrderUuid(orderUuid)
                                  .withExternalUuid(user.externalUuid)
                                  .withUserId(userId)
                                  .withWalletId(walletId)
                                  .withLastUpdated(lastUpdated)
                              ) :+ createOrUpdatePaymentAccount
                            )
                            .thenRun(_ => CardNotPreRegistered ~> replyTo)
                        } else {
                          Effect
                            .persist(
                              createOrUpdatePaymentAccount
                            )
                            .thenRun(_ => CardNotPreRegistered ~> replyTo)
                        }
                    }
                  case _ =>
                    Effect
                      .persist(
                        PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(
                            paymentAccount
                              .copy(
                                user = PaymentAccount.User.NaturalUser(
                                  user.withUserId(userId).withPaymentUserType(PaymentUserType.PAYER)
                                )
                              )
                              .withLastUpdated(lastUpdated)
                          )
                          .withLastUpdated(lastUpdated)
                      )
                      .thenRun(_ => CardNotPreRegistered ~> replyTo)
                }
              case _ =>
                Effect
                  .persist(
                    PaymentAccountUpsertedEvent.defaultInstance
                      .withDocument(
                        paymentAccount
                          .withNaturalUser(
                            user.withPaymentUserType(PaymentUserType.PAYER)
                          )
                          .withLastUpdated(lastUpdated)
                      )
                      .withLastUpdated(lastUpdated)
                  )
                  .thenRun(_ => CardNotPreRegistered ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => CardNotPreRegistered ~> replyTo)
        }

      case cmd: PreAuthorizeCard =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.getNaturalUser.userId match {
              case Some(userId) =>
                (registrationId match {
                  case Some(id) =>
                    createCard(id, registrationData)
                  case _ =>
                    paymentAccount.cards
                      .find(card => card.active.getOrElse(true) && !card.expired)
                      .map(_.id)
                }) match {
                  case Some(cardId) =>
                    preAuthorizeCard(
                      Some(
                        PreAuthorizationTransaction.defaultInstance
                          .withCardId(cardId)
                          .withAuthorId(userId)
                          .withDebitedAmount(debitedAmount)
                          .withOrderUuid(orderUuid)
                          .copy(
                            ipAddress = ipAddress,
                            browserInfo = browserInfo
                          )
                      )
                    ) match {
                      case Some(transaction) =>
                        handleCardPreAuthorization(
                          entityId,
                          orderUuid,
                          replyTo,
                          paymentAccount,
                          registerCard,
                          transaction
                        )
                      case _ => // pre authorization failed
                        Effect.none.thenRun(_ => CardNotPreAuthorized ~> replyTo)
                    }
                  case _ => // no card id
                    Effect.none.thenRun(_ => CardNotPreAuthorized ~> replyTo)
                }
              case _ => // no userId
                Effect.none.thenRun(_ => CardNotPreAuthorized ~> replyTo)
            }
          case _ => // no payment account
            Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PreAuthorizeCardFor3DS => // 3DS
        import cmd._
        state match {
          case Some(paymentAccount) =>
            loadCardPreAuthorized(orderUuid, preAuthorizationId) match {
              case Some(transaction) =>
                handleCardPreAuthorization(
                  entityId,
                  orderUuid,
                  replyTo,
                  paymentAccount,
                  registerCard,
                  transaction
                )
              case _ => Effect.none.thenRun(_ => CardNotPreAuthorized ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: CancelPreAuthorization =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.transactions.find(_.id == cardPreAuthorizedTransactionId) match {
              case Some(_) =>
                val preAuthorizationCanceled =
                  cancelPreAuthorization(orderUuid, cardPreAuthorizedTransactionId)
                Effect
                  .persist(
                    PreAuthorizationCanceledEvent.defaultInstance
                      .withLastUpdated(now())
                      .withOrderUuid(orderUuid)
                      .withDebitedAccount(paymentAccount.externalUuid)
                      .withCardPreAuthorizedTransactionId(cardPreAuthorizedTransactionId)
                      .withPreAuthorizationCanceled(preAuthorizationCanceled)
                  )
                  .thenRun(_ => PreAuthorizationCanceled(preAuthorizationCanceled) ~> replyTo)
              case _ => // should never be the case
                Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PayInWithCardPreAuthorized =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.transactions.find(_.id == preAuthorizationId) match {
              case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
              case Some(transaction)
                  if Seq(
                    Transaction.TransactionStatus.TRANSACTION_CREATED,
                    Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                  ).contains(transaction.status) =>
                // load credited payment account
                paymentDao.loadPaymentAccount(creditedAccount) complete () match {
                  case Success(s) =>
                    s match {
                      case Some(creditedPaymentAccount) =>
                        creditedPaymentAccount.walletId match {
                          case Some(creditedWalletId) =>
                            payInWithCardPreAuthorized(
                              Some(
                                PayInWithCardPreAuthorizedTransaction.defaultInstance
                                  .withCardPreAuthorizedTransactionId(preAuthorizationId)
                                  .withAuthorId(transaction.authorId)
                                  .withDebitedAmount(transaction.amount)
                                  .withCurrency(transaction.currency)
                                  .withOrderUuid(transaction.orderUuid)
                                  .withCreditedWalletId(creditedWalletId)
                              )
                            ) match {
                              case Some(transaction) =>
                                handlePayIn(
                                  entityId,
                                  transaction.orderUuid,
                                  replyTo,
                                  paymentAccount,
                                  registerCard = false,
                                  transaction
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
                          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                        }
                      case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                    }
                  case Failure(_) => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => IllegalTransactionStatus ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PayIn =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentType match {
              case Transaction.PaymentType.CARD =>
                paymentAccount.userId match {
                  case Some(userId) =>
                    (registrationId match {
                      case Some(id) =>
                        createCard(id, registrationData)
                      case _ =>
                        paymentAccount.cards
                          .find(card => card.active.getOrElse(true) && !card.expired)
                          .map(_.id)
                    }) match {
                      case Some(cardId) =>
                        // load credited payment account
                        paymentDao.loadPaymentAccount(creditedAccount) complete () match {
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
                                          .withCardId(cardId)
                                          .withPaymentType(paymentType)
                                          .withStatementDescriptor(
                                            statementDescriptor.getOrElse(PayInStatementDescriptor)
                                          )
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
                                          transaction
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
                      case _ =>
                        Effect.none.thenRun(_ =>
                          PayInFailed(
                            "",
                            Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                            "no card"
                          ) ~> replyTo
                        )
                    }
                  case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
                }

              case _ =>
                Effect
                  .persist(
                    broadcastEvent(
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

      case cmd: PayInFor3DS =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            loadPayIn(orderUuid, transactionId, None) match {
              case Some(transaction) =>
                handlePayIn(entityId, orderUuid, replyTo, paymentAccount, registerCard, transaction)
              case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: Refund =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            (paymentAccount.transactions.find(_.id == payInTransactionId) match {
              case None => loadPayIn(orderUuid, payInTransactionId, None)
              case some => some
            }) match {
              case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
              case Some(transaction)
                  if Seq(
                    Transaction.TransactionStatus.TRANSACTION_CREATED,
                    Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                  ).contains(transaction.status) =>
                if (refundAmount > transaction.amount) {
                  Effect.none.thenRun(_ => IllegalTransactionAmount ~> replyTo)
                } else {
                  refund(
                    Some(
                      RefundTransaction.defaultInstance
                        .withOrderUuid(orderUuid)
                        .withRefundAmount(refundAmount)
                        .withCurrency(currency)
                        .withAuthorId(transaction.authorId)
                        .withReasonMessage(reasonMessage)
                        .withPayInTransactionId(payInTransactionId)
                        .withInitializedByClient(initializedByClient)
                    )
                  ) match {
                    case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
                    case Some(transaction) =>
                      keyValueDao.addKeyValue(transaction.id, entityId)
                      val lastUpdated = now()
                      val updatedPaymentAccount = paymentAccount
                        .withTransactions(
                          paymentAccount.transactions.filterNot(_.id == transaction.id)
                          :+ transaction
                        )
                        .withLastUpdated(lastUpdated)
                      val upsertedEvent =
                        PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(updatedPaymentAccount)
                          .withLastUpdated(lastUpdated)
                      transaction.status match {
                        case Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON =>
                          log.error(
                            "Order-{} could not be refunded: {} -> {}",
                            orderUuid,
                            transaction.id,
                            asJson(transaction)
                          )
                          Effect
                            .persist(
                              broadcastEvent(
                                RefundFailedEvent.defaultInstance
                                  .withOrderUuid(orderUuid)
                                  .withResultMessage(transaction.resultMessage)
                                  .withTransaction(transaction)
                              ) :+ upsertedEvent
                            )
                            .thenRun(_ =>
                              RefundFailed(
                                "",
                                Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                                transaction.resultMessage
                              ) ~> replyTo
                            )
                        case _ =>
                          if (
                            transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated
                          ) {
                            log.info(
                              "Order-{} refunded: {} -> {}",
                              orderUuid,
                              transaction.id,
                              asJson(transaction)
                            )
                            Effect
                              .persist(
                                broadcastEvent(
                                  RefundedEvent.defaultInstance
                                    .withOrderUuid(orderUuid)
                                    .withLastUpdated(lastUpdated)
                                    .withDebitedAccount(paymentAccount.externalUuid)
                                    .withDebitedAmount(transaction.amount)
                                    .withRefundedAmount(refundAmount)
                                    .withCurrency(currency)
                                    .withRefundTransactionId(transaction.id)
                                    .withPayInTransactionId(payInTransactionId)
                                    .withReasonMessage(reasonMessage)
                                    .withInitializedByClient(initializedByClient)
                                    .withPaymentType(transaction.paymentType)
                                ) :+ upsertedEvent
                              )
                              .thenRun(_ => Refunded(transaction.id, transaction.status) ~> replyTo)
                          } else {
                            log.info(
                              "Order-{} could not be refunded: {} -> {}",
                              orderUuid,
                              transaction.id,
                              asJson(transaction)
                            )
                            Effect
                              .persist(
                                broadcastEvent(
                                  RefundFailedEvent.defaultInstance
                                    .withOrderUuid(orderUuid)
                                    .withResultMessage(transaction.resultMessage)
                                    .withTransaction(transaction)
                                ) :+ upsertedEvent
                              )
                              .thenRun(_ =>
                                RefundFailed(
                                  transaction.id,
                                  transaction.status,
                                  transaction.resultMessage
                                ) ~> replyTo
                              )
                          }
                      }
                    case _ =>
                      log.error(
                        "Order-{} could not be refunded: no transaction returned by provider",
                        orderUuid
                      )
                      Effect
                        .persist(
                          broadcastEvent(
                            RefundFailedEvent.defaultInstance
                              .withOrderUuid(orderUuid)
                              .withResultMessage("no transaction returned by provider")
                          )
                        )
                        .thenRun(_ =>
                          RefundFailed(
                            "",
                            Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                            "no transaction returned by provider"
                          ) ~> replyTo
                        )
                  }
                }
              case _ => Effect.none.thenRun(_ => IllegalTransactionStatus ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PayOut =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.userId match {
              case Some(userId) =>
                paymentAccount.walletId match {
                  case Some(walletId) =>
                    paymentAccount.bankAccount.flatMap(_.id) match {
                      case Some(bankAccountId) =>
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
                          )
                        ) match {
                          case Some(transaction) =>
                            keyValueDao.addKeyValue(transaction.id, entityId)
                            val lastUpdated = now()
                            val updatedPaymentAccount = paymentAccount
                              .withTransactions(
                                paymentAccount.transactions.filterNot(_.id == transaction.id)
                                :+ transaction
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
                                  broadcastEvent(
                                    PayOutFailedEvent.defaultInstance
                                      .withOrderUuid(orderUuid)
                                      .withResultMessage(transaction.resultMessage)
                                      .withTransaction(transaction)
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
                                  broadcastEvent(
                                    PaidOutEvent.defaultInstance
                                      .withOrderUuid(orderUuid)
                                      .withLastUpdated(lastUpdated)
                                      .withCreditedAccount(paymentAccount.externalUuid)
                                      .withCreditedAmount(creditedAmount)
                                      .withFeesAmount(feesAmount)
                                      .withCurrency(currency)
                                      .withTransactionId(transaction.id)
                                      .withPaymentType(transaction.paymentType)
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
                                  broadcastEvent(
                                    PayOutFailedEvent.defaultInstance
                                      .withOrderUuid(orderUuid)
                                      .withResultMessage(transaction.resultMessage)
                                      .withTransaction(transaction)
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
                                broadcastEvent(
                                  PayOutFailedEvent.defaultInstance
                                    .withOrderUuid(orderUuid)
                                    .withResultMessage("no transaction returned by provider")
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
                        Effect.none.thenRun(_ =>
                          PayOutFailed(
                            "",
                            Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                            "no bank account"
                          ) ~> replyTo
                        )
                    }
                  case _ =>
                    Effect.none.thenRun(_ =>
                      PayOutFailed(
                        "",
                        Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                        "no wallet id"
                      ) ~> replyTo
                    )
                }
              case _ =>
                Effect.none.thenRun(_ =>
                  PayOutFailed(
                    "",
                    Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED,
                    "no payment provider user id"
                  ) ~> replyTo
                )
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: Transfer =>
        import cmd._
        state match {
          case Some(paymentAccount) => // debited account
            var maybeCreditedPaymentAccount: Option[PaymentAccount] = None
            transfer(paymentAccount.userId match {
              case Some(authorId) =>
                paymentAccount.walletId match {
                  case Some(debitedWalletId) =>
                    // load credited payment account
                    paymentDao.loadPaymentAccount(creditedAccount) complete () match {
                      case Success(s) =>
                        maybeCreditedPaymentAccount = s
                        maybeCreditedPaymentAccount match {
                          case Some(creditedPaymentAccount) => // credited account
                            creditedPaymentAccount.userId match {
                              case Some(creditedUserId) =>
                                creditedPaymentAccount.walletId match {
                                  case Some(creditedWalletId) =>
                                    Some(
                                      TransferTransaction.defaultInstance
                                        .withDebitedAmount(debitedAmount)
                                        .withFeesAmount(feesAmount)
                                        .withCurrency(currency)
                                        .withAuthorId(authorId)
                                        .withDebitedWalletId(debitedWalletId)
                                        .withCreditedUserId(creditedUserId)
                                        .withCreditedWalletId(creditedWalletId)
                                        .copy(
                                          orderUuid = orderUuid,
                                          externalReference = externalReference
                                        )
                                    )
                                  case _ => None
                                }
                              case _ => None
                            }
                          case _ => None
                        }
                      case Failure(_) => None
                    }
                  case _ => None
                }
              case _ => None
            }) match {
              case Some(transaction) =>
                keyValueDao.addKeyValue(transaction.id, entityId)
                val lastUpdated = now()
                val updatedPaymentAccount = paymentAccount
                  .withTransactions(
                    paymentAccount.transactions.filterNot(_.id == transaction.id)
                    :+ transaction
                  )
                  .withLastUpdated(lastUpdated)
                if (
                  transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated
                ) {
                  val payOutTransactionId =
                    if (payOutRequired) {
                      paymentDao.payOut(
                        orderUuid.getOrElse(""),
                        creditedAccount,
                        debitedAmount,
                        feesAmount = 0 /* fees have already been applied with Transfer */
                      ) complete () match {
                        case Success(s) =>
                          s match {
                            case Right(r) => Some(r.transactionId)
                            case _        => None
                          }
                        case Failure(_) => None
                      }
                    } else {
                      None
                    }
                  Effect
                    .persist(
                      broadcastEvent(
                        TransferedEvent.defaultInstance
                          .withFeesAmount(feesAmount)
                          .withDebitedAmount(debitedAmount)
                          .withDebitedAccount(paymentAccount.externalUuid)
                          .withCurrency(currency)
                          .withLastUpdated(lastUpdated)
                          .withCreditedAccount(
                            maybeCreditedPaymentAccount
                              .map(_.externalUuid)
                              .getOrElse(creditedAccount)
                          )
                          .withTransactionId(transaction.id)
                          .withTransactionStatus(transaction.status)
                          .withPaymentType(transaction.paymentType)
                          .copy(
                            payOutTransactionId = payOutTransactionId,
                            orderUuid = orderUuid,
                            externalReference = externalReference
                          )
                      ) :+
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(updatedPaymentAccount)
                        .withLastUpdated(lastUpdated)
                    )
                    .thenRun(_ =>
                      {
                        Transferred(transaction.id, transaction.status, payOutTransactionId)
                      } ~> replyTo
                    )
                } else {
                  Effect
                    .persist(
                      broadcastEvent(
                        TransferFailedEvent.defaultInstance
                          .withDebitedAccount(paymentAccount.externalUuid)
                          .withResultMessage(transaction.resultMessage)
                          .withTransaction(transaction)
                          .copy(externalReference = externalReference)
                      ) :+
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(updatedPaymentAccount)
                        .withLastUpdated(lastUpdated)
                    )
                    .thenRun(_ =>
                      TransferFailed(
                        transaction.id,
                        transaction.status,
                        transaction.resultMessage
                      ) ~> replyTo
                    )
                }
              case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: CreateMandate =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.userId match {
              case Some(creditedUserId) =>
                paymentAccount.bankAccount.flatMap(_.id) match {
                  case Some(bankAccountId) =>
                    // check if a mandate is already associated to this bank account and activated
                    if (paymentAccount.mandateActivated) {
                      Effect.none.thenRun(_ => MandateAlreadyExists ~> replyTo)
                    } else if (paymentAccount.documents.exists(!_.status.isKycDocumentValidated)) {
                      Effect.none.thenRun(_ => MandateNotCreated ~> replyTo)
                    } else {
                      addMandate(
                        entityId,
                        replyTo,
                        creditedAccount,
                        paymentAccount,
                        creditedUserId,
                        bankAccountId
                      )
                    }
                  case _ => Effect.none.thenRun(_ => BankAccountNotFound ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case _: CancelMandate =>
        state match {
          case Some(paymentAccount) =>
            if (paymentAccount.mandateExists && paymentAccount.mandateRequired) {
              Effect
                .persist(
                  broadcastEvent(
                    MandateCancelationFailedEvent(paymentAccount.externalUuid, now())
                  )
                )
                .thenRun(_ => MandateNotCanceled ~> replyTo)
            } else {
              paymentAccount.bankAccount match {
                case Some(bankAccount) =>
                  bankAccount.mandateId match {
                    case Some(mandateId) =>
                      cancelMandate(mandateId) match {
                        case Some(_) =>
                          keyValueDao.removeKeyValue(mandateId)
                          val lastUpdated = now()
                          val updatePaymentAccount = paymentAccount
                            .copy(
                              bankAccount = paymentAccount.bankAccount.map(
                                _.copy(
                                  mandateId = None,
                                  mandateStatus = None
                                )
                              )
                            )
                            .withLastUpdated(lastUpdated)
                          Effect
                            .persist(
                              broadcastEvent(
                                MandateUpdatedEvent.defaultInstance
                                  .withExternalUuid(paymentAccount.externalUuid)
                                  .withLastUpdated(lastUpdated)
                                  .withBankAccountId(bankAccount.getId)
                                  .copy(
                                    mandateId = None,
                                    mandateStatus = None
                                  )
                              ) :+
                              PaymentAccountUpsertedEvent.defaultInstance
                                .withLastUpdated(lastUpdated)
                                .withDocument(updatePaymentAccount)
                            )
                            .thenRun(_ => MandateCanceled ~> replyTo)

                        case _ => Effect.none.thenRun(_ => MandateNotCanceled ~> replyTo)
                      }
                    case _ => Effect.none.thenRun(_ => MandateNotFound ~> replyTo)
                  }
                case _ => Effect.none.thenRun(_ => BankAccountNotFound ~> replyTo)
              }
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: UpdateMandateStatus =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.userId match {
              case Some(userId) =>
                paymentAccount.bankAccount.flatMap(_.id) match {
                  case Some(bankAccountId) =>
                    loadMandate(Some(mandateId), userId, bankAccountId) match {
                      case Some(report) =>
                        val internalStatus =
                          if (environment != "prod") {
                            status.getOrElse(report.status)
                          } else {
                            report.status
                          }
                        val lastUpdated = now()
                        val updatePaymentAccount = paymentAccount
                          .copy(
                            bankAccount = paymentAccount.bankAccount.map(
                              _.withMandateId(mandateId).withMandateStatus(internalStatus)
                            )
                          )
                          .withLastUpdated(lastUpdated)
                        Effect
                          .persist(
                            broadcastEvent(
                              MandateUpdatedEvent.defaultInstance
                                .withExternalUuid(paymentAccount.externalUuid)
                                .withLastUpdated(lastUpdated)
                                .withMandateId(mandateId)
                                .withMandateStatus(internalStatus)
                                .withBankAccountId(bankAccountId)
                            ) :+
                            PaymentAccountUpsertedEvent.defaultInstance
                              .withLastUpdated(lastUpdated)
                              .withDocument(updatePaymentAccount)
                          )
                          .thenRun(_ =>
                            MandateStatusUpdated(report.withStatus(internalStatus)) ~> replyTo
                          )

                      case _ => Effect.none.thenRun(_ => MandateStatusNotUpdated ~> replyTo)
                    }
                  case _ => Effect.none.thenRun(_ => BankAccountNotFound ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: DirectDebit =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
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
                                .withStatementDescriptor(statementDescriptor)
                                .copy(externalReference = externalReference)
                            )
                          ) match {
                            case Some(transaction) =>
                              keyValueDao.addKeyValue(transaction.id, entityId)
                              val lastUpdated = now()
                              val updatedPaymentAccount = paymentAccount
                                .withTransactions(
                                  paymentAccount.transactions.filterNot(_.id == transaction.id)
                                  :+ transaction
                                )
                                .withLastUpdated(lastUpdated)
                              if (
                                transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated
                              ) {
                                Effect
                                  .persist(
                                    broadcastEvent(
                                      DirectDebitedEvent.defaultInstance
                                        .withLastUpdated(lastUpdated)
                                        .withCreditedAccount(paymentAccount.externalUuid)
                                        .withDebitedAmount(debitedAmount)
                                        .withFeesAmount(feesAmount)
                                        .withCurrency(currency)
                                        .withTransactionId(transaction.id)
                                        .withTransactionStatus(transaction.status)
                                        .copy(externalReference = externalReference)
                                    ) :+
                                    PaymentAccountUpsertedEvent.defaultInstance
                                      .withLastUpdated(lastUpdated)
                                      .withDocument(updatedPaymentAccount)
                                  )
                                  .thenRun(_ =>
                                    DirectDebited(transaction.id, transaction.status) ~> replyTo
                                  )
                              } else {
                                Effect
                                  .persist(
                                    broadcastEvent(
                                      DirectDebitFailedEvent.defaultInstance
                                        .withCreditedAccount(paymentAccount.externalUuid)
                                        .withResultMessage(transaction.resultMessage)
                                        .withTransaction(transaction)
                                        .copy(externalReference = externalReference)
                                    ) :+
                                    PaymentAccountUpsertedEvent.defaultInstance
                                      .withLastUpdated(lastUpdated)
                                      .withDocument(updatedPaymentAccount)
                                  )
                                  .thenRun(_ =>
                                    DirectDebitFailed(
                                      transaction.id,
                                      transaction.status,
                                      transaction.resultMessage
                                    ) ~> replyTo
                                  )
                              }
                            case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
                          }
                        } else {
                          Effect.none.thenRun(_ => IllegalMandateStatus ~> replyTo)
                        }
                      case _ => Effect.none.thenRun(_ => MandateNotFound ~> replyTo)
                    }
                  case _ => Effect.none.thenRun(_ => WalletNotFound ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadDirectDebitTransaction =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.transactions.find(_.id == directDebitTransactionId) match {
              case Some(transaction) =>
                paymentAccount.walletId match {
                  case Some(creditedWalletId) =>
                    val transactionDate: LocalDate = transaction.createdDate
                    directDebitTransaction(
                      transaction.creditedWalletId.getOrElse(creditedWalletId),
                      transaction.id,
                      transactionDate.minusDays(1)
                    ) match {
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
                            :+ updatedTransaction
                          )
                          .withLastUpdated(lastUpdated)
                        if (t.status.isTransactionSucceeded || t.status.isTransactionCreated) {
                          Effect
                            .persist(
                              broadcastEvent(
                                DirectDebitedEvent.defaultInstance
                                  .withLastUpdated(lastUpdated)
                                  .withCreditedAccount(paymentAccount.externalUuid)
                                  .withDebitedAmount(updatedTransaction.amount)
                                  .withFeesAmount(updatedTransaction.fees)
                                  .withCurrency(updatedTransaction.currency)
                                  .withTransactionId(updatedTransaction.id)
                                  .withTransactionStatus(updatedTransaction.status)
                                  .copy(externalReference = updatedTransaction.externalReference)
                              ) :+
                              PaymentAccountUpsertedEvent.defaultInstance
                                .withLastUpdated(lastUpdated)
                                .withDocument(updatedPaymentAccount)
                            )
                            .thenRun(_ =>
                              DirectDebited(transaction.id, transaction.status) ~> replyTo
                            )
                        } else {
                          Effect
                            .persist(
                              broadcastEvent(
                                DirectDebitFailedEvent.defaultInstance
                                  .withCreditedAccount(paymentAccount.externalUuid)
                                  .withResultMessage(updatedTransaction.resultMessage)
                                  .withTransaction(updatedTransaction)
                                  .copy(externalReference = transaction.externalReference)
                              ) :+
                              PaymentAccountUpsertedEvent.defaultInstance
                                .withLastUpdated(lastUpdated)
                                .withDocument(updatedPaymentAccount)
                            )
                            .thenRun(_ =>
                              DirectDebitFailed(
                                transaction.id,
                                transaction.status,
                                transaction.resultMessage
                              ) ~> replyTo
                            )
                        }
                      case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
                    }
                  case _ => Effect.none.thenRun(_ => WalletNotFound ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PayInFirstRecurring =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.recurryingPayments.find(
              _.getId == cmd.recurringPayInRegistrationId
            ) match {
              case Some(recurringPayment) =>
                createRecurringCardPayment(
                  RecurringPaymentTransaction.defaultInstance
                    .withExternalUuid(paymentAccount.externalUuid)
                    .withRecurringPayInRegistrationId(cmd.recurringPayInRegistrationId)
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

      case cmd: PayInFirstRecurringFor3DS =>
        state match {
          case Some(paymentAccount) =>
            import cmd._
            paymentAccount.recurryingPayments.find(_.getId == recurringPayInRegistrationId) match {
              case Some(recurringPayment) =>
                loadPayIn("", transactionId, Some(recurringPayInRegistrationId)) match {
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

      case cmd: TriggerSchedule4Payment =>
        import cmd.schedule._
        if (key.startsWith(nextRecurringPayment)) {
          state match {
            case Some(paymentAccount) =>
              val recurringPaymentRegistrationId = key.split("#").last
              Effect.none.thenRun(_ => {
                context.self ! PayNextRecurring(
                  recurringPaymentRegistrationId,
                  paymentAccount.externalUuid
                )
                Schedule4PaymentTriggered(cmd.schedule) ~> replyTo
              })
            case _ =>
              Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
          }
        } else {
          Effect.none.thenRun(_ => Schedule4PaymentNotTriggered ~> replyTo)
        }

      case cmd: PayNextRecurring =>
        state match {
          case Some(paymentAccount) =>
            import cmd._
            paymentAccount.recurryingPayments.find(
              _.getId == recurringPaymentRegistrationId
            ) match {
              case Some(recurringPayment) =>
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
                            .withStatementDescriptor(statementDescriptor.getOrElse("")) // TODO
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

      case _: LoadPaymentAccount =>
        state match {
          case Some(paymentAccount) =>
            Effect.none.thenRun(_ => PaymentAccountLoaded(paymentAccount) ~> replyTo)
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadTransaction =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.transactions.find(_.id == transactionId) match {
              case Some(transaction) =>
                Effect.none.thenRun(_ => TransactionLoaded(transaction) ~> replyTo)
              case _ => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: CreateOrUpdateBankAccount =>
        if (cmd.bankAccount.wrongIban) {
          Effect.none.thenRun(_ => WrongIban ~> replyTo)
        } else if (cmd.bankAccount.wrongBic) {
          Effect.none.thenRun(_ => WrongBic ~> replyTo)
        }
//        else if(cmd.bankAccount.wrongOwnerName) {
//          Effect.none.thenRun(_ => WrongOwnerName ~> replyTo)
//        }
        else if (cmd.bankAccount.wrongOwnerAddress) {
          Effect.none.thenRun(_ => WrongOwnerAddress ~> replyTo)
        } else {
          (state match {
            case None =>
              cmd.user match {
                case Some(user) =>
                  loadPaymentAccount(entityId, None, user)
                case _ => None
              }
            case some => some
          }) match {
            case Some(paymentAccount) =>
              import cmd._

              val updatedUser: PaymentAccount.User =
                user match {
                  case None => paymentAccount.user
                  case Some(updatedUser) =>
                    if (paymentAccount.user.isLegalUser && updatedUser.isLegalUser) {
                      val previousLegalUser = paymentAccount.getLegalUser
                      val updatedLegalUser = updatedUser.legalUser.get
                      PaymentAccount.User.LegalUser(
                        updatedLegalUser.copy(
                          legalRepresentative = updatedLegalUser.legalRepresentative
                            .copy(
                              userId = previousLegalUser.legalRepresentative.userId,
                              walletId = previousLegalUser.legalRepresentative.walletId
                            )
                            .withPaymentUserType(
                              updatedLegalUser.legalRepresentative.paymentUserType
                                .getOrElse(PaymentUserType.COLLECTOR)
                            ),
                          uboDeclaration = previousLegalUser.uboDeclaration,
                          lastAcceptedTermsOfPSP = previousLegalUser.lastAcceptedTermsOfPSP
                        )
                      )
                    } else if (updatedUser.isLegalUser) {
                      val updatedLegalUser = updatedUser.legalUser.get
                      PaymentAccount.User.LegalUser(
                        updatedLegalUser.copy(
                          legalRepresentative =
                            updatedLegalUser.legalRepresentative.withPaymentUserType(
                              updatedLegalUser.legalRepresentative.paymentUserType.getOrElse(
                                PaymentUserType.COLLECTOR
                              )
                            )
                        )
                      )
                    } else if (paymentAccount.user.isNaturalUser && updatedUser.isNaturalUser) {
                      val previousNaturalUser = paymentAccount.getNaturalUser
                      val updatedNaturalUser = updatedUser.naturalUser.get
                      PaymentAccount.User.NaturalUser(
                        updatedNaturalUser
                          .copy(
                            userId = previousNaturalUser.userId,
                            walletId = previousNaturalUser.walletId
                          )
                          .withPaymentUserType(
                            updatedNaturalUser.paymentUserType.getOrElse(PaymentUserType.COLLECTOR)
                          )
                      )
                    } else if (updatedUser.isNaturalUser) {
                      val updatedNaturalUser = updatedUser.naturalUser.get
                      PaymentAccount.User.NaturalUser(
                        updatedNaturalUser.withPaymentUserType(
                          updatedNaturalUser.paymentUserType.getOrElse(PaymentUserType.COLLECTOR)
                        )
                      )
                    } else {
                      updatedUser
                    }
                }

              val lastUpdated = now()

              val shouldUpdateIban =
                !paymentAccount.bankAccount.exists(_.checkIfSameIban(bankAccount.iban))

              val iban = {
                if (!shouldUpdateIban) {
                  paymentAccount.bankAccount match {
                    case Some(previous) => previous.iban
                    case _              => bankAccount.iban
                  }
                } else {
                  bankAccount.iban
                }
              }

              val shouldUpdateBic =
                shouldUpdateIban || !paymentAccount.bankAccount.exists(
                  _.checkIfSameBic(bankAccount.bic)
                )

              val bic = {
                if (!shouldUpdateBic) {
                  paymentAccount.bankAccount match {
                    case Some(previous) => previous.bic
                    case _              => bankAccount.bic
                  }
                } else {
                  bankAccount.bic
                }
              }

              var updatedPaymentAccount =
                paymentAccount
                  .withUser(updatedUser)
                  .withBankAccount(
                    bankAccount
                      .copy(
                        mandateId = paymentAccount.bankAccount.flatMap(_.mandateId),
                        mandateStatus = paymentAccount.bankAccount.flatMap(_.mandateStatus)
                      )
                      .withIban(iban)
                      .withBic(bic)
                  )
                  .withLastUpdated(lastUpdated)

              updatedPaymentAccount.user.legalUser match {
                case Some(legalUser) if legalUser.wrongSiret =>
                  Effect.none.thenRun(_ => WrongSiret ~> replyTo)
                case Some(legalUser) if legalUser.legalName.trim.isEmpty =>
                  Effect.none.thenRun(_ => LegalNameRequired ~> replyTo)
                case Some(legalUser) if legalUser.wrongLegalRepresentativeAddress =>
                  Effect.none.thenRun(_ => WrongLegalRepresentativeAddress ~> replyTo)
                case Some(legalUser) if legalUser.wrongHeadQuartersAddress =>
                  Effect.none.thenRun(_ => WrongHeadQuartersAddress ~> replyTo)
                case Some(legalUser)
                    if legalUser.lastAcceptedTermsOfPSP.isEmpty && !acceptedTermsOfPSP.getOrElse(
                      false
                    ) =>
                  Effect.none.thenRun(_ => AcceptedTermsOfPSPRequired ~> replyTo)
                case None if paymentAccount.emptyUser =>
                  Effect.none.thenRun(_ => UserRequired ~> replyTo)
                case _ =>
                  val shouldCreateUser = paymentAccount.userId.isEmpty

                  val shouldUpdateUserType = !shouldCreateUser && {
                    if (paymentAccount.legalUser) { // previous user is a legal user
                      !updatedPaymentAccount.legalUser || // update to natural user
                      !paymentAccount.checkIfSameLegalUserType(
                        updatedPaymentAccount.legalUserType
                      ) // update legal user type
                    } else { // previous user is a natural user
                      updatedPaymentAccount.legalUser // update to legal user
                    }
                  }

                  val shouldUpdateKYC =
                    shouldUpdateUserType ||
                    paymentAccount.maybeUser.map(_.firstName).getOrElse("") !=
                      updatedPaymentAccount.maybeUser.map(_.firstName).getOrElse("") ||
                      paymentAccount.maybeUser.map(_.lastName).getOrElse("") !=
                      updatedPaymentAccount.maybeUser.map(_.lastName).getOrElse("") ||
                      paymentAccount.maybeUser.map(_.birthday).getOrElse("") !=
                      updatedPaymentAccount.maybeUser.map(_.birthday).getOrElse("")

                  val shouldUpdateUser = shouldUpdateKYC ||
                    (updatedPaymentAccount.legalUser &&
                    (updatedPaymentAccount.getLegalUser.legalName != paymentAccount.getLegalUser.legalName ||
                    updatedPaymentAccount.getLegalUser.siret != paymentAccount.getLegalUser.siret ||
                    !updatedPaymentAccount.getLegalUser.legalRepresentativeAddress.equals(
                      paymentAccount.getLegalUser.legalRepresentativeAddress
                    ) ||
                    !updatedPaymentAccount.getLegalUser.headQuartersAddress.equals(
                      paymentAccount.getLegalUser.headQuartersAddress
                    ) ||
                    updatedPaymentAccount.getLegalUser.legalRepresentative.email
                      != paymentAccount.getLegalUser.legalRepresentative.email ||
                      updatedPaymentAccount.getLegalUser.legalRepresentative.nationality
                      != paymentAccount.getLegalUser.legalRepresentative.nationality ||
                      updatedPaymentAccount.getLegalUser.legalRepresentative.countryOfResidence
                      != paymentAccount.getLegalUser.legalRepresentative.countryOfResidence)) ||
                    (!updatedPaymentAccount.legalUser && (updatedPaymentAccount.getNaturalUser.email != paymentAccount.getNaturalUser.email ||
                    updatedPaymentAccount.getNaturalUser.nationality != paymentAccount.getNaturalUser.nationality ||
                    updatedPaymentAccount.getNaturalUser.countryOfResidence
                      != paymentAccount.getNaturalUser.countryOfResidence))

                  //val shouldCreateOrUpdateUser = shouldCreateUser || shouldUpdateUser

                  val shouldCreateBankAccount = paymentAccount.bankAccount.isEmpty ||
                    paymentAccount.bankAccount.flatMap(_.id).isEmpty

                  val shouldUpdateBankAccount = !shouldCreateBankAccount && (
                    paymentAccount.bankAccount
                      .map(_.ownerName)
                      .getOrElse("") != bankAccount.ownerName ||
                    !paymentAccount.getBankAccount.ownerAddress.equals(bankAccount.ownerAddress) ||
                    shouldUpdateIban ||
                    shouldUpdateBic /*||
                      shouldUpdateUser*/
                  )

                  val shouldCreateOrUpdateBankAccount =
                    shouldCreateBankAccount || shouldUpdateBankAccount

                  val documents: List[KycDocument] = initDocuments(updatedPaymentAccount)

                  val shouldUpdateDocuments = shouldUpdateKYC

                  val shouldCancelMandate = shouldUpdateBankAccount &&
                    paymentAccount.bankAccount.flatMap(_.mandateId).isDefined

                  val shouldCreateUboDeclaration = updatedPaymentAccount.legalUser &&
                    updatedPaymentAccount.getLegalUser.uboDeclarationRequired &&
                    updatedPaymentAccount.getLegalUser.uboDeclaration.isEmpty

                  (paymentAccount.userId match {
                    case None =>
                      createOrUpdatePaymentAccount(Some(updatedPaymentAccount))
                    case Some(_) if shouldUpdateUser =>
                      if (shouldUpdateUserType) {
                        createOrUpdatePaymentAccount(Some(updatedPaymentAccount.resetUserId(None)))
                      } else {
                        createOrUpdatePaymentAccount(Some(updatedPaymentAccount))
                      }
                    case some => some
                  }) match {
                    case Some(userId) =>
                      keyValueDao.addKeyValue(userId, entityId)
                      updatedPaymentAccount = updatedPaymentAccount.resetUserId(Some(userId))
                      (paymentAccount.walletId match {
                        case None =>
                          createOrUpdateWallet(
                            Some(userId),
                            "EUR",
                            updatedPaymentAccount.externalUuid,
                            None
                          )
                        case Some(_) if shouldUpdateUserType =>
                          createOrUpdateWallet(
                            Some(userId),
                            "EUR",
                            updatedPaymentAccount.externalUuid,
                            None
                          )
                        case some => some
                      }) match {
                        case Some(walletId) =>
                          keyValueDao.addKeyValue(walletId, entityId)
                          updatedPaymentAccount =
                            updatedPaymentAccount.resetWalletId(Some(walletId))
                          (paymentAccount.bankAccount.flatMap(_.id) match {
                            case None =>
                              createOrUpdateBankAccount(
                                updatedPaymentAccount.resetBankAccountId().bankAccount
                              )
                            case Some(_) if shouldCreateOrUpdateBankAccount =>
                              createOrUpdateBankAccount(
                                updatedPaymentAccount.resetBankAccountId().bankAccount
                              )
                            case some => some
                          }) match {
                            case Some(bankAccountId) =>
                              keyValueDao.addKeyValue(bankAccountId, entityId)
                              updatedPaymentAccount =
                                updatedPaymentAccount.resetBankAccountId(Some(bankAccountId))

                              var events: List[ExternalSchedulerEvent] = List.empty

                              if (shouldCreateBankAccount) {
                                updatedPaymentAccount = updatedPaymentAccount
                                  .withBankAccount(
                                    updatedPaymentAccount.getBankAccount
                                      .withCreatedDate(lastUpdated)
                                      .withLastUpdated(lastUpdated)
                                  )
                              } else if (shouldUpdateBankAccount) {
                                updatedPaymentAccount = updatedPaymentAccount
                                  .withBankAccount(
                                    updatedPaymentAccount.getBankAccount
                                      .withLastUpdated(lastUpdated)
                                  )
                              }

                              // BankAccountUpdatedEvent
                              events = events ++
                                broadcastEvent(
                                  BankAccountUpdatedEvent.defaultInstance
                                    .withExternalUuid(updatedPaymentAccount.externalUuid)
                                    .withLastUpdated(lastUpdated)
                                    .withUserId(userId)
                                    .withWalletId(walletId)
                                    .withBankAccountId(bankAccountId)
                                )

                              if (
                                updatedPaymentAccount.legalUser && acceptedTermsOfPSP.getOrElse(
                                  false
                                )
                              ) {
                                updatedPaymentAccount = updatedPaymentAccount.withLegalUser(
                                  updatedPaymentAccount.getLegalUser.withLastAcceptedTermsOfPSP(
                                    lastUpdated
                                  )
                                )
                                // TermsOfPSPAcceptedEvent
                                events = events ++
                                  broadcastEvent(
                                    TermsOfPSPAcceptedEvent.defaultInstance
                                      .withExternalUuid(updatedPaymentAccount.externalUuid)
                                      .withLastUpdated(lastUpdated)
                                      .withLastAcceptedTermsOfPSP(lastUpdated)
                                  )
                              }

                              if (shouldCreateUboDeclaration) {
                                createDeclaration(userId) match {
                                  case Some(uboDeclaration) =>
                                    keyValueDao.addKeyValue(uboDeclaration.id, entityId)
                                    updatedPaymentAccount = updatedPaymentAccount.withLegalUser(
                                      updatedPaymentAccount.getLegalUser.withUboDeclaration(
                                        uboDeclaration
                                      )
                                    )
                                    // UboDeclarationUpdatedEvent
                                    events = events ++
                                      broadcastEvent(
                                        UboDeclarationUpdatedEvent.defaultInstance
                                          .withExternalUuid(updatedPaymentAccount.externalUuid)
                                          .withLastUpdated(lastUpdated)
                                          .withUboDeclaration(uboDeclaration)
                                      )
                                  case _ =>
                                    log.warn(s"Could not create ubo declaration for user $userId")
                                }
                              }

                              if (shouldUpdateDocuments) { // TODO we should rely on the payment provider to update document status
                                updatedPaymentAccount = updatedPaymentAccount
                                  /*.withDocuments(
                                      documents.map(
                                        _.copy(
                                          lastUpdated = Some(lastUpdated),
                                          status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED,
                                          refusedReasonType = None,
                                          refusedReasonMessage = None
                                        )
                                      )
                                    )*/
                                  .withDocuments(documents)
                                  .withPaymentAccountStatus(
                                    PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO
                                  )

                                // PaymentAccountStatusUpdatedEvent
                                events = events ++
                                  broadcastEvent(
                                    PaymentAccountStatusUpdatedEvent.defaultInstance
                                      .withExternalUuid(updatedPaymentAccount.externalUuid)
                                      .withLastUpdated(lastUpdated)
                                      .withPaymentAccountStatus(
                                        updatedPaymentAccount.paymentAccountStatus
                                      )
                                  )
                              } else {
                                updatedPaymentAccount =
                                  updatedPaymentAccount.withDocuments(documents)
                              }

                              if (shouldCancelMandate) {
                                updatedPaymentAccount = updatedPaymentAccount.copy(
                                  bankAccount = updatedPaymentAccount.bankAccount.map(
                                    _.copy(
                                      mandateId = None,
                                      mandateStatus = None
                                    )
                                  )
                                )

                                // MandateUpdatedEvent
                                events = events ++
                                  broadcastEvent(
                                    MandateUpdatedEvent.defaultInstance
                                      .withExternalUuid(updatedPaymentAccount.externalUuid)
                                      .withLastUpdated(lastUpdated)
                                      .withBankAccountId(bankAccountId)
                                      .copy(
                                        mandateId = None,
                                        mandateStatus = None
                                      )
                                  )
                              }

                              // DocumentsUpdatedEvent
                              events = events ++
                                broadcastEvent(
                                  DocumentsUpdatedEvent.defaultInstance
                                    .withExternalUuid(updatedPaymentAccount.externalUuid)
                                    .withLastUpdated(lastUpdated)
                                    .withDocuments(updatedPaymentAccount.documents)
                                )

                              val encodedPaymentAccount =
                                updatedPaymentAccount.copy(
                                  bankAccount = updatedPaymentAccount.bankAccount.map(
                                    _.encode(shouldUpdateBic, shouldUpdateIban)
                                  )
                                )

                              Effect.persist(
                                events :+
                                PaymentAccountUpsertedEvent.defaultInstance
                                  .withDocument(encodedPaymentAccount)
                                  .withLastUpdated(lastUpdated)
                              ) thenRun (_ =>
                                BankAccountCreatedOrUpdated(
                                  shouldCreateUser,
                                  shouldUpdateUserType,
                                  shouldUpdateKYC,
                                  shouldUpdateUser,
                                  shouldCreateBankAccount,
                                  shouldUpdateBankAccount,
                                  shouldUpdateDocuments,
                                  shouldCancelMandate,
                                  shouldCreateUboDeclaration,
                                  encodedPaymentAccount.view
                                ) ~> replyTo
                              )

                            case _ =>
                              Effect.none.thenRun(_ => BankAccountNotCreatedOrUpdated ~> replyTo)
                          }
                        case _ =>
                          Effect.none.thenRun(_ => BankAccountNotCreatedOrUpdated ~> replyTo)
                      }
                    case _ => Effect.none.thenRun(_ => BankAccountNotCreatedOrUpdated ~> replyTo)
                  }
              }

            case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
          }
        }

      case cmd: AddKycDocument =>
        import cmd._
        state match {
          case Some(paymentAccount) if paymentAccount.hasAcceptedTermsOfPSP =>
            paymentAccount.userId match {
              case Some(userId) =>
                addDocument(userId, entityId, pages, kycDocumentType) match {
                  case Some(documentId) =>
                    paymentAccount.documents.find(_.`type` == kycDocumentType).flatMap(_.id) match {
                      case Some(previous) if previous != documentId =>
                        keyValueDao.removeKeyValue(previous)
                      case _ =>
                    }
                    keyValueDao.addKeyValue(documentId, entityId)

                    val lastUpdated = now()

                    val updatedDocument =
                      paymentAccount.documents
                        .find(_.`type` == kycDocumentType)
                        .getOrElse(
                          KycDocument.defaultInstance
                            .withCreatedDate(lastUpdated)
                            .withType(kycDocumentType)
                        )
                        .withLastUpdated(lastUpdated)
                        .withId(documentId)
                        .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
                        .copy(
                          refusedReasonType = None,
                          refusedReasonMessage = None
                        )

                    val newDocuments =
                      paymentAccount.documents.filterNot(_.`type` == kycDocumentType) :+
                      updatedDocument

                    Effect
                      .persist(
                        broadcastEvent(
                          DocumentsUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withDocuments(newDocuments)
                        ) ++ broadcastEvent(
                          DocumentUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withDocument(updatedDocument)
                        ) :+ PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(
                            paymentAccount.withDocuments(newDocuments).withLastUpdated(lastUpdated)
                          )
                          .withLastUpdated(lastUpdated)
                      )
                      .thenRun(_ => KycDocumentAdded(documentId) ~> replyTo)

                  case _ => Effect.none.thenRun(_ => KycDocumentNotAdded ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => KycDocumentNotAdded ~> replyTo)
            }
          case None => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
          case _    => Effect.none.thenRun(_ => AcceptedTermsOfPSPRequired ~> replyTo)
        }

      case cmd: UpdateKycDocumentStatus =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.documents.find(_.id.getOrElse("") == kycDocumentId) match {
              case Some(document) =>
                val documentStatusUpdated =
                  updateDocumentStatus(
                    paymentAccount,
                    document,
                    kycDocumentId,
                    status
                  )
                Effect
                  .persist(
                    documentStatusUpdated._2
                  )
                  .thenRun(_ => KycDocumentStatusUpdated(documentStatusUpdated._1) ~> replyTo)
              case _ => Effect.none.thenRun(_ => KycDocumentStatusNotUpdated ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: LoadKycDocumentStatus =>
        state match {
          case Some(paymentAccount) =>
            import cmd._
            paymentAccount.documents.find(_.`type` == kycDocumentType) match {
              case Some(document) =>
                if (document.status.isKycDocumentValidationAsked) {
                  val documentStatusUpdated =
                    updateDocumentStatus(
                      paymentAccount,
                      document,
                      document.id.getOrElse(""),
                      None
                    )
                  Effect
                    .persist(
                      documentStatusUpdated._2
                    )
                    .thenRun(_ => KycDocumentStatusLoaded(documentStatusUpdated._1) ~> replyTo)
                } else {
                  Effect.none.thenRun(_ =>
                    KycDocumentStatusLoaded(
                      KycDocumentValidationReport.defaultInstance
                        .withId(document.id.getOrElse(""))
                        .withStatus(document.status)
                        .copy(
                          refusedReasonType = document.refusedReasonType,
                          refusedReasonMessage = document.refusedReasonMessage
                        )
                    ) ~> replyTo
                  )
                }
              case _ => Effect.none.thenRun(_ => KycDocumentStatusNotLoaded ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: CreateOrUpdateUbo =>
        state match {
          case Some(paymentAccount) =>
            var declarationCreated: Boolean = false
            def createInternalDeclaration(): Option[UboDeclaration] = {
              createDeclaration(paymentAccount.userId.getOrElse("")) match {
                case Some(declaration) =>
                  declarationCreated = true
                  keyValueDao.addKeyValue(declaration.id, entityId)
                  Some(declaration)
                case _ => None
              }
            }
            (paymentAccount.getLegalUser.uboDeclaration match {
              case None =>
                createInternalDeclaration()
              case Some(uboDeclaration) =>
                if (uboDeclaration.status.isUboDeclarationRefused) {
                  createInternalDeclaration()
                } else {
                  Some(uboDeclaration)
                }
            }) match {
              case Some(declaration) =>
                import cmd._
                var events: List[ExternalSchedulerEvent] = List.empty

                val lastUpdated = now()

                if (declarationCreated) {
                  events = events ++
                    broadcastEvent(
                      UboDeclarationUpdatedEvent.defaultInstance
                        .withExternalUuid(paymentAccount.externalUuid)
                        .withLastUpdated(lastUpdated)
                        .withUboDeclaration(declaration)
                    )
                }

                createOrUpdateUBO(paymentAccount.userId.getOrElse(""), declaration.id, ubo) match {
                  case Some(ubo) =>
                    Effect
                      .persist(
                        events :+
                        PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(
                            paymentAccount
                              .withLegalUser(
                                paymentAccount.getLegalUser
                                  .withUboDeclaration(
                                    declaration
                                      .withUbos(declaration.ubos.filterNot(_.id == ubo.id) :+ ubo)
                                  )
                              )
                              .withLastUpdated(lastUpdated)
                          )
                          .withLastUpdated(lastUpdated)
                      )
                      .thenRun(_ => UboCreatedOrUpdated(ubo) ~> replyTo)

                  case _ => Effect.persist(events).thenRun(_ => UboNotCreatedOrUpdated ~> replyTo)
                }

              case _ => Effect.none.thenRun(_ => UboDeclarationNotFound ~> replyTo)
            }

          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case _: ValidateUboDeclaration =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.getLegalUser.uboDeclaration match {
              case None => Effect.none.thenRun(_ => UboDeclarationNotFound ~> replyTo)
              case Some(uboDeclaration)
                  if uboDeclaration.status.isUboDeclarationCreated ||
                    uboDeclaration.status.isUboDeclarationIncomplete || uboDeclaration.status.isUboDeclarationRefused =>
                validateDeclaration(paymentAccount.userId.getOrElse(""), uboDeclaration.id) match {
                  case Some(declaration) =>
                    val updatedUbo = declaration.withUbos(uboDeclaration.ubos)
                    val lastUpdated = now()
                    Effect
                      .persist(
                        broadcastEvent(
                          UboDeclarationUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withUboDeclaration(updatedUbo)
                        ) :+
                        PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(
                            paymentAccount
                              .withLegalUser(
                                paymentAccount.getLegalUser.withUboDeclaration(updatedUbo)
                              )
                              .withLastUpdated(lastUpdated)
                          )
                          .withLastUpdated(lastUpdated)
                      )
                      .thenRun(_ => UboDeclarationAskedForValidation ~> replyTo)
                  case _ => Effect.none.thenRun(_ => UboDeclarationNotAskedForValidation ~> replyTo)
                }
              case _ => Effect.none.thenRun(_ => UboDeclarationNotAskedForValidation ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case _: GetUboDeclaration =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.getLegalUser.uboDeclaration match {
              case Some(uboDeclaration) =>
                Effect.none.thenRun(_ => UboDeclarationLoaded(uboDeclaration) ~> replyTo)
              case _ => Effect.none.thenRun(_ => UboDeclarationNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: UpdateUboDeclarationStatus =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.getLegalUser.uboDeclaration match {
              // check if this update is related to the actual ubo declaration waiting for validation
              case Some(uboDeclaration)
                  if cmd.uboDeclarationId == uboDeclaration.id &&
                    uboDeclaration.status.isUboDeclarationValidationAsked =>
                import cmd._
                getDeclaration(paymentAccount.userId.getOrElse(""), uboDeclarationId) match {
                  case Some(declaration) =>
                    val internalStatus = {
                      if (environment != "prod") {
                        status.getOrElse(declaration.status)
                      } else {
                        declaration.status
                      }
                    }
                    var events: List[ExternalSchedulerEvent] = List.empty
                    val lastUpdated = now()
                    var updatedDeclaration =
                      declaration.withStatus(internalStatus)
                    if (internalStatus.isUboDeclarationRefused) {
                      updatedDeclaration = updatedDeclaration.withUbos(
                        Seq.empty /*updatedDeclaration.ubos.map(_.copy(id = None))*/
                      )
                    }
                    var updatedPaymentAccount = paymentAccount
                      .withLegalUser(
                        paymentAccount.getLegalUser.withUboDeclaration(updatedDeclaration)
                      )
                      .withLastUpdated(lastUpdated)
                    events = events ++
                      broadcastEvent(
                        UboDeclarationUpdatedEvent.defaultInstance
                          .withExternalUuid(paymentAccount.externalUuid)
                          .withLastUpdated(lastUpdated)
                          .withUboDeclaration(updatedDeclaration)
                      )
                    if (
                      internalStatus.isUboDeclarationIncomplete || internalStatus.isUboDeclarationRefused
                    ) {
                      events = events ++
                        broadcastEvent(
                          PaymentAccountStatusUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withPaymentAccountStatus(
                              PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO
                            )
                        )
                      updatedPaymentAccount = updatedPaymentAccount
                        .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
                    } else if (
                      internalStatus.isUboDeclarationValidated && paymentAccount.documentsValidated
                    ) {
                      events = events ++
                        broadcastEvent(
                          PaymentAccountStatusUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
                        )
                      updatedPaymentAccount = updatedPaymentAccount
                        .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
                    }
                    Effect
                      .persist(
                        events :+
                        PaymentAccountUpsertedEvent.defaultInstance
                          .withDocument(updatedPaymentAccount)
                          .withLastUpdated(lastUpdated)
                      )
                      .thenRun(_ => UboDeclarationStatusUpdated ~> replyTo)
                  case _ =>
                    Effect.none.thenRun(_ => UboDeclarationStatusNotUpdated ~> replyTo)
                }
              case _ =>
                Effect.none.thenRun(_ => UboDeclarationStatusNotUpdated ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: ValidateRegularUser =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val lastUpdated = now()

            var events: List[ExternalSchedulerEvent] =
              broadcastEvent(
                PaymentAccountStatusUpdatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
              ) ++
              broadcastEvent(
                RegularUserValidatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withUserId(userId)
              )

            var updatedPaymentAccount = paymentAccount
              .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
              .withLastUpdated(lastUpdated)
            updatedPaymentAccount.getLegalUser.uboDeclaration match {
              case Some(uboDeclaration) =>
                val declaration =
                  uboDeclaration.withStatus(
                    UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED
                  )
                updatedPaymentAccount = updatedPaymentAccount.withLegalUser(
                  updatedPaymentAccount.getLegalUser.withUboDeclaration(declaration)
                )
                events = events ++
                  broadcastEvent(
                    UboDeclarationUpdatedEvent.defaultInstance
                      .withExternalUuid(paymentAccount.externalUuid)
                      .withLastUpdated(lastUpdated)
                      .withUboDeclaration(declaration)
                  )
              case _ =>
            }

            if (!updatedPaymentAccount.documentsValidated) {
              updatedPaymentAccount = updatedPaymentAccount.withDocuments(
                updatedPaymentAccount.documents.map(
                  _.withLastUpdated(lastUpdated)
                    .copy(
                      status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
                    )
                )
              )
              events = events ++
                broadcastEvent(
                  DocumentsUpdatedEvent.defaultInstance
                    .withExternalUuid(paymentAccount.externalUuid)
                    .withLastUpdated(lastUpdated)
                    .withDocuments(updatedPaymentAccount.documents)
                )
            }

            Effect
              .persist(
                events :+
                PaymentAccountUpsertedEvent.defaultInstance
                  .withDocument(updatedPaymentAccount)
                  .withLastUpdated(lastUpdated)
              )
              .thenRun(_ => RegularUserValidated ~> replyTo)

          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: InvalidateRegularUser =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val lastUpdated = now()

            var events: List[ExternalSchedulerEvent] =
              broadcastEvent(
                PaymentAccountStatusUpdatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
              ) ++
              broadcastEvent(
                RegularUserInvalidatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withUserId(userId)
              )

            var updatedPaymentAccount = paymentAccount
              .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
              .withLastUpdated(lastUpdated)

            Effect
              .persist(
                events :+
                PaymentAccountUpsertedEvent.defaultInstance
                  .withDocument(updatedPaymentAccount)
                  .withLastUpdated(lastUpdated)
              )
              .thenRun(_ => RegularUserInvalidated ~> replyTo)

          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case _: LoadBankAccount =>
        state match {
          case Some(paymentAccount) =>
            Effect.none.thenRun(_ =>
              (paymentAccount.bankAccount match {
                case Some(bank) => BankAccountLoaded(bank)
                case _          => BankAccountNotFound
              }) ~> replyTo
            )
          case _ => Effect.none.thenRun(_ => BankAccountNotFound ~> replyTo)
        }

      case cmd: DeleteBankAccount =>
        state match {
          case Some(_)
              if PaymentSettings.DisableBankAccountDeletion && !cmd.force.getOrElse(false) =>
            Effect.none.thenRun(_ => BankAccountDeletionDisabled ~> replyTo)

          case Some(paymentAccount) =>
            if (
              paymentAccount.mandateActivated && (
//              paymentAccount.transactions.exists(t => t.`type`.isDirectDebit) ||
                paymentAccount.recurryingPayments
                  .exists(r => r.`type`.isDirectDebit && r.nextPaymentDate.isDefined)
              )
            ) {
              Effect.none.thenRun(_ => BankAccountNotDeleted ~> replyTo)
            } else {
              paymentAccount.bankAccount match {
                case Some(bankAccount) =>
                  bankAccount.id match {
                    case Some(bankAccountId) =>
                      keyValueDao.removeKeyValue(bankAccountId)
                    case _ =>
                  }
                  val lastUpdated = now()
                  var updatedPaymentAccount = paymentAccount
                    .copy(bankAccount = None)
                    .withLastUpdated(lastUpdated)
                  var events: List[ExternalSchedulerEvent] = {
                    broadcastEvent(
                      BankAccountDeletedEvent.defaultInstance
                        .withExternalUuid(paymentAccount.externalUuid)
                        .withLastUpdated(lastUpdated)
                    )
                  }
                  updatedPaymentAccount.getLegalUser.uboDeclaration match {
                    case Some(declaration) =>
                      keyValueDao.removeKeyValue(declaration.id)
                      updatedPaymentAccount = updatedPaymentAccount
                        .withLegalUser(
                          updatedPaymentAccount.getLegalUser.copy(uboDeclaration = None)
                        )
                      events = events ++
                        broadcastEvent(
                          UboDeclarationUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .clearUboDeclaration
                        )
                    case _ =>
                  }

                  updatedPaymentAccount = updatedPaymentAccount
                    .withDocuments(
                      updatedPaymentAccount.documents.map(
                        _.copy(
                          id = None,
                          lastUpdated = Some(lastUpdated),
                          status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                        )
                      )
                    )
                    .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
                  events = events ++
                    broadcastEvent(
                      DocumentsUpdatedEvent.defaultInstance
                        .withExternalUuid(paymentAccount.externalUuid)
                        .withLastUpdated(lastUpdated)
                        .withDocuments(updatedPaymentAccount.documents)
                    ) ++
                    broadcastEvent(
                      PaymentAccountStatusUpdatedEvent.defaultInstance
                        .withExternalUuid(paymentAccount.externalUuid)
                        .withLastUpdated(lastUpdated)
                        .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
                    )

                  Effect
                    .persist(
                      events :+
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(updatedPaymentAccount)
                        .withLastUpdated(lastUpdated)
                    )
                    .thenRun(_ => BankAccountDeleted ~> replyTo)
                case _ => Effect.none.thenRun(_ => BankAccountNotFound ~> replyTo)
              }
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case _: LoadCards =>
        state match {
          case Some(paymentAccount) =>
            Effect.none.thenRun(_ => CardsLoaded(paymentAccount.cards) ~> replyTo)
          case _ => Effect.none.thenRun(_ => CardsNotLoaded ~> replyTo)
        }

      case cmd: DisableCard =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.cards.find(_.id == cmd.cardId) match {
              case Some(card) if card.getActive =>
                paymentAccount.recurryingPayments.find(r =>
                  r.`type`.isCard && r.getCardId == cmd.cardId &&
                  r.nextPaymentDate.isDefined
                ) match {
                  case Some(_) =>
                    Effect.none.thenRun(_ => CardNotDisabled ~> replyTo)
                  case _ =>
                    disableCard(cmd.cardId) match {
                      case Some(_) =>
                        val lastUpdated = now()
                        Effect
                          .persist(
                            PaymentAccountUpsertedEvent.defaultInstance
                              .withDocument(
                                paymentAccount
                                  .withCards(
                                    paymentAccount.cards.filterNot(_.id == cmd.cardId) :+ card
                                      .withActive(false)
                                  )
                                  .withLastUpdated(lastUpdated)
                              )
                              .withLastUpdated(lastUpdated)
                          )
                          .thenRun(_ => CardDisabled ~> replyTo)
                      case _ => Effect.none.thenRun(_ => CardNotDisabled ~> replyTo)
                    }
                }
              case _ => Effect.none.thenRun(_ => CardNotDisabled ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => CardNotDisabled ~> replyTo)
        }

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
                                    broadcastEvent(
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
                      broadcastEvent(
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
              _.getId == cmd.recurringPayInRegistrationId
            ) match {
              case Some(recurringPayment) if recurringPayment.`type`.isCard =>
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
                  cmd.recurringPayInRegistrationId,
                  cardId,
                  cmd.status
                ) match {
                  case Some(result) =>
                    val lastUpdated = now()
                    val updatedPaymentAccount =
                      paymentAccount
                        .withRecurryingPayments(
                          paymentAccount.recurryingPayments
                            .filterNot(_.getId == cmd.recurringPayInRegistrationId) :+
                          recurringPayment.withCardStatus(result.status)
                        )
                        .withLastUpdated(lastUpdated)
                    Effect
                      .persist(
                        broadcastEvent(
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
                                    s"$nextRecurringPayment#${cmd.recurringPayInRegistrationId}"
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

      case _ => super.handleCommand(entityId, state, command, replyTo, timers)
    }
  }

  private def handleNextRecurringPaymentFailure(
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
        broadcastEvent(
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

  private def addMandate(
    entityId: String,
    replyTo: Option[ActorRef[PaymentResult]],
    creditedAccount: String,
    paymentAccount: PaymentAccount,
    creditedUserId: String,
    bankAccountId: String
  )(implicit context: ActorContext[_]): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    mandate(creditedAccount, creditedUserId, bankAccountId) match {
      case Some(mandateResult) =>
        if (mandateResult.status.isMandateFailed) {
          Effect.none.thenRun(_ =>
            MandateCreationFailed(
              mandateResult.resultCode.getOrElse(""),
              mandateResult.resultMessage.getOrElse("")
            ) ~> replyTo
          )
        } else {
          keyValueDao.addKeyValue(mandateResult.id, entityId)
          val lastUpdated = now()
          val updatePaymentAccount = paymentAccount
            .copy(
              bankAccount = paymentAccount.bankAccount.map(
                _.withMandateId(mandateResult.id).withMandateStatus(mandateResult.status)
              )
            )
            .withLastUpdated(lastUpdated)
          Effect
            .persist(
              broadcastEvent(
                MandateUpdatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withMandateId(mandateResult.id)
                  .withMandateStatus(mandateResult.status)
                  .withBankAccountId(bankAccountId)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withLastUpdated(lastUpdated)
                .withDocument(updatePaymentAccount)
            )
            .thenRun(_ =>
              (mandateResult.status match {
                case BankAccount.MandateStatus.MANDATE_CREATED =>
                  MandateConfirmationRequired(mandateResult.redirectUrl)
                case _ => MandateCreated
              }) ~> replyTo
            )
        }
      case _ => Effect.none.thenRun(_ => MandateNotCreated ~> replyTo)
    }
  }

  /** @param state
    *   - current state
    * @param event
    *   - event to handle
    * @return
    *   new state
    */
  override def handleEvent(state: Option[PaymentAccount], event: ExternalSchedulerEvent)(implicit
    context: ActorContext[_]
  ): Option[PaymentAccount] =
    event match {
      case _ => super.handleEvent(state, event)
    }

  private[this] def loadPaymentAccount(
    entityId: String,
    state: Option[PaymentAccount],
    user: PaymentAccount.User
  )(implicit system: ActorSystem[_], log: Logger): Option[PaymentAccount] = {
    val pa = PaymentAccount.defaultInstance.withUser(user)
    val uuid = pa.externalUuidWithProfile
    state match {
      case None =>
        keyValueDao.lookupKeyValue(uuid) complete () match {
          case Success(s) =>
            s match {
              case Some(t) if t != entityId =>
                log.warn(
                  s"another payment account entity $t has already been associated with this uuid $uuid"
                )
                None
              case _ =>
                keyValueDao.addKeyValue(uuid, entityId)
                Some(pa.withUuid(entityId).withCreatedDate(now()))
            }
          case Failure(f) =>
            log.error(f.getMessage, f)
            None
        }
      case Some(paymentAccount) =>
        if (paymentAccount.externalUuid != pa.externalUuid) {
          log.warn(
            s"the payment account entity $entityId has already been associated with another external uuid ${paymentAccount.externalUuid}"
          )
          None
        } else {
          keyValueDao.addKeyValue(uuid, entityId)
          Some(paymentAccount)
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
    var updatedPaymentAccount =
      paymentAccount
        .withTransactions(
          paymentAccount.transactions
            .filterNot(_.id == transaction.id) :+ transaction
        )
        .withLastUpdated(lastUpdated)
    transaction.status match {
      case Transaction.TransactionStatus.TRANSACTION_CREATED
          if transaction.redirectUrl.isDefined => // 3ds
        Effect
          .persist(
            PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated)
          )
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
              broadcastEvent(
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
                .withLastUpdated(lastUpdated)
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
              broadcastEvent(
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
              } :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
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

  private[this] def handlePayIn(
    entityId: String,
    orderUuid: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    registerCard: Boolean,
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
    var updatedPaymentAccount =
      paymentAccount
        .withTransactions(
          paymentAccount.transactions
            .filterNot(_.id == transaction.id) :+ transaction
        )
        .withLastUpdated(lastUpdated)
    transaction.status match {
      case Transaction.TransactionStatus.TRANSACTION_CREATED
          if transaction.redirectUrl.isDefined => // 3ds
        Effect
          .persist(
            PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated)
          )
          .thenRun(_ => PaymentRedirection(transaction.redirectUrl.get) ~> replyTo)
      case _ =>
        if (transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated) {
          log.debug("Order-{} paid in: {} -> {}", orderUuid, transaction.id, asJson(transaction))
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
                      broadcastEvent(
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
          Effect
            .persist(
              registerCardEvents ++
              broadcastEvent(
                PaidInEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withTransactionId(transaction.id)
                  .withDebitedAccount(paymentAccount.externalUuid)
                  .withDebitedAmount(transaction.amount)
                  .withLastUpdated(lastUpdated)
                  .withCardId(transaction.cardId.getOrElse(""))
                  .withPaymentType(transaction.paymentType)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
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
              broadcastEvent(
                PayInFailedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withResultMessage(transaction.resultMessage)
                  .withTransaction(transaction)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
            )
            .thenRun(_ =>
              PayInFailed(transaction.id, transaction.status, transaction.resultMessage) ~> replyTo
            )
        }
    }
  }

  private[this] def handleCardPreAuthorization(
    entityId: String,
    orderUuid: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    registerCard: Boolean,
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
    var updatedPaymentAccount =
      paymentAccount
        .withTransactions(
          paymentAccount.transactions
            .filterNot(_.id == transaction.id) :+ transaction
        )
        .withLastUpdated(lastUpdated)
    transaction.status match {
      case Transaction.TransactionStatus.TRANSACTION_CREATED
          if transaction.redirectUrl.isDefined => // 3ds
        Effect
          .persist(
            PaymentAccountUpsertedEvent.defaultInstance
              .withDocument(updatedPaymentAccount)
              .withLastUpdated(lastUpdated)
          )
          .thenRun(_ =>
            PaymentRedirection(
              transaction.redirectUrl.get
            ) ~> replyTo
          )
      case _ =>
        if (transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated) {
          log.debug(
            "Order-{} pre authorized: {} -> {}",
            orderUuid,
            transaction.id,
            asJson(transaction)
          )
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
                      broadcastEvent(
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
          Effect
            .persist(
              registerCardEvents ++
              broadcastEvent(
                CardPreAuthorizedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withTransactionId(transaction.id)
                  .withCardId(transaction.getCardId)
                  .withDebitedAccount(paymentAccount.externalUuid)
                  .withDebitedAmount(transaction.amount)
                  .withLastUpdated(lastUpdated)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
            )
            .thenRun(_ => CardPreAuthorized(transaction.id) ~> replyTo)
        } else {
          log.error(
            "Order-{} could not be pre authorized: {} -> {}",
            orderUuid,
            transaction.id,
            asJson(transaction)
          )
          Effect
            .persist(
              broadcastEvent(
                CardPreAuthorizationFailedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withResultMessage(transaction.resultMessage)
                  .withTransaction(transaction)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
            )
            .thenRun(_ => CardPreAuthorizationFailed(transaction.resultMessage) ~> replyTo)
        }
    }
  }

  private[this] def updateDocumentStatus(
    paymentAccount: PaymentAccount,
    document: KycDocument,
    documentId: String,
    maybeStatus: Option[KycDocument.KycDocumentStatus] = None
  )(implicit
    system: ActorSystem[_]
  ): (KycDocumentValidationReport, List[ExternalSchedulerEvent]) = {
    var events: List[ExternalSchedulerEvent] = List.empty
    val lastUpdated = now()

    val userId = paymentAccount.userId.getOrElse("")

    val report = loadDocumentStatus(userId, documentId)

    val internalStatus =
      if (environment != "prod") {
        maybeStatus.getOrElse(report.status)
      } else {
        report.status
      }

    val updatedDocument =
      document
        .withLastUpdated(lastUpdated)
        .withStatus(internalStatus)
        .copy(
          refusedReasonType = report.refusedReasonType,
          refusedReasonMessage = report.refusedReasonMessage
        )

    events = events ++
      broadcastEvent(
        DocumentUpdatedEvent.defaultInstance
          .withExternalUuid(paymentAccount.externalUuid)
          .withLastUpdated(lastUpdated)
          .withDocument(updatedDocument)
      )

    val newDocuments =
      paymentAccount.documents.filterNot(_.id.getOrElse("") == documentId) :+ updatedDocument

    var updatedPaymentAccount =
      paymentAccount.withDocuments(newDocuments).withLastUpdated(lastUpdated)

    events = events ++
      broadcastEvent(
        DocumentsUpdatedEvent.defaultInstance
          .withExternalUuid(paymentAccount.externalUuid)
          .withLastUpdated(lastUpdated)
          .withDocuments(newDocuments)
      )

    if (
      updatedPaymentAccount.documentsValidated && paymentAccount.getLegalUser.uboDeclarationValidated
    ) {
      if (!paymentAccount.paymentAccountStatus.isCompteOk) {
        events = events ++
          broadcastEvent(
            PaymentAccountStatusUpdatedEvent.defaultInstance
              .withExternalUuid(paymentAccount.externalUuid)
              .withLastUpdated(lastUpdated)
              .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
          )
        updatedPaymentAccount = updatedPaymentAccount
          .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
      }
    } else if (internalStatus.isKycDocumentRefused) {
      events = events ++
        broadcastEvent(
          PaymentAccountStatusUpdatedEvent.defaultInstance
            .withExternalUuid(paymentAccount.externalUuid)
            .withLastUpdated(lastUpdated)
            .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
        )
      updatedPaymentAccount = updatedPaymentAccount
        .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
    } else if (internalStatus.isKycDocumentOutOfDate && !paymentAccount.documentOutdated) {
      events = events ++
        broadcastEvent(
          PaymentAccountStatusUpdatedEvent.defaultInstance
            .withExternalUuid(paymentAccount.externalUuid)
            .withLastUpdated(lastUpdated)
            .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
        )
      updatedPaymentAccount = updatedPaymentAccount
        .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
    }

    (
      report,
      events :+
      PaymentAccountUpsertedEvent.defaultInstance
        .withDocument(updatedPaymentAccount)
        .withLastUpdated(lastUpdated)
    )
  }

  private[this] def initDocuments(paymentAccount: PaymentAccount): List[KycDocument] = {
    var newDocuments: List[KycDocument] = paymentAccount.documents.toList
    newDocuments = List(
      newDocuments
        .find(_.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF)
        .getOrElse(
          KycDocument.defaultInstance.copy(
            `type` = KycDocument.KycDocumentType.KYC_IDENTITY_PROOF,
            status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
          )
        )
    ) ++ newDocuments.filterNot(_.`type` == KycDocument.KycDocumentType.KYC_IDENTITY_PROOF)
    paymentAccount.legalUserType match {
      case Some(lpt) =>
        lpt match {
          case LegalUserType.SOLETRADER =>
            newDocuments = List(
              newDocuments
                .find(_.`type` == KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF)
                .getOrElse(
                  KycDocument.defaultInstance.copy(
                    `type` = KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
                    status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                  )
                )
            ) ++ newDocuments.filterNot(
              _.`type` == KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF
            )
          case LegalUserType.BUSINESS =>
            newDocuments = List(
              newDocuments
                .find(_.`type` == KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF)
                .getOrElse(
                  KycDocument.defaultInstance.copy(
                    `type` = KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
                    status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                  )
                ),
              newDocuments
                .find(_.`type` == KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION)
                .getOrElse(
                  KycDocument.defaultInstance.copy(
                    `type` = KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION,
                    status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                  )
                ),
              newDocuments
                .find(_.`type` == KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION)
                .getOrElse(
                  KycDocument.defaultInstance.copy(
                    `type` = KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION,
                    status = KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
                  )
                )
            ) ++ newDocuments.filterNot(d =>
              Seq(
                KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
                KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION,
                KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION
              ).contains(d.`type`)
            )
          case _ =>
        }

      case _ =>
        newDocuments = newDocuments.filterNot(d =>
          Seq(
            KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF,
            KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION,
            KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION
          ).contains(d.`type`)
        )
    }

    newDocuments
  }
}
