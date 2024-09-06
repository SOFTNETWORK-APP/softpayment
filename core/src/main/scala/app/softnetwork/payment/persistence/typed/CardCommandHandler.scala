package app.softnetwork.payment.persistence.typed

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentEvents.{
  CardPreRegisteredEvent,
  CardRegisteredEvent,
  PaymentAccountUpsertedEvent,
  WalletRegisteredEvent
}
import app.softnetwork.payment.message.PaymentMessages.{
  CancelPreAuthorization,
  CardCommand,
  CardDisabled,
  CardNotDisabled,
  CardNotPreAuthorized,
  CardNotPreRegistered,
  CardPreAuthorizationFailed,
  CardPreAuthorized,
  CardPreRegistered,
  CardsLoaded,
  CardsNotLoaded,
  DisableCard,
  LoadCards,
  PayInWithCardPreAuthorized,
  PayInWithCardPreAuthorizedFailed,
  PaymentAccountNotFound,
  PaymentRedirection,
  PaymentResult,
  PreAuthorizationCanceled,
  PreAuthorizeCard,
  PreAuthorizeCardCallback,
  PreRegisterCard,
  TransactionNotFound
}
import app.softnetwork.payment.message.TransactionEvents.{
  CardPreAuthorizationFailedEvent,
  CardPreAuthorizedEvent,
  PayInFailedEvent,
  PreAuthorizationCanceledEvent
}
import app.softnetwork.payment.model.NaturalUser.NaturalUserType
import app.softnetwork.payment.model.{
  CardOwner,
  PayInTransaction,
  PaymentAccount,
  PreAuthorizationTransaction,
  Transaction
}
import app.softnetwork.persistence.now
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent
import app.softnetwork.serialization.asJson
import app.softnetwork.time._
import org.slf4j.Logger

import scala.util.{Failure, Success}

trait CardCommandHandler
    extends EntityCommandHandler[CardCommand, PaymentAccount, ExternalSchedulerEvent, PaymentResult]
    with PaymentCommandHandler
    with PayInHandler
    with Completion {

  override def apply(
    entityId: String,
    state: Option[PaymentAccount],
    command: CardCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[CardCommand]
  )(implicit
    context: ActorContext[CardCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)

    command match {
      case cmd: PreRegisterCard =>
        import cmd._
        var registerWallet: Boolean = false
        loadPaymentAccount(entityId, state, PaymentAccount.User.NaturalUser(user), clientId) match {
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
                    val paymentAccountUpsertedEvent =
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(
                          paymentAccount
                            .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
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
                        .withLastUpdated(lastUpdated)
                    preRegisterCard(Some(userId), currency, user.externalUuid) match {
                      case Some(cardPreRegistration) =>
                        keyValueDao.addKeyValue(cardPreRegistration.id, entityId)
                        val walletEvents: List[ExternalSchedulerEvent] =
                          if (registerWallet) {
                            List(
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
                            List(
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
                            ) ++ walletEvents :+ paymentAccountUpsertedEvent
                          )
                          .thenRun(_ => CardPreRegistered(cardPreRegistration) ~> replyTo)
                      case _ =>
                        if (registerWallet) {
                          Effect
                            .persist(
                              List(
                                WalletRegisteredEvent.defaultInstance
                                  .withOrderUuid(orderUuid)
                                  .withExternalUuid(user.externalUuid)
                                  .withUserId(userId)
                                  .withWalletId(walletId)
                                  .withLastUpdated(lastUpdated)
                              ) :+ paymentAccountUpsertedEvent
                            )
                            .thenRun(_ => CardNotPreRegistered ~> replyTo)
                        } else {
                          Effect
                            .persist(
                              paymentAccountUpsertedEvent
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
                                  user.withUserId(userId).withNaturalUserType(NaturalUserType.PAYER)
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
                            user.withNaturalUserType(NaturalUserType.PAYER)
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
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
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
                    val creditedUserId: Option[String] =
                      creditedAccount match {
                        case Some(account) =>
                          paymentDao.loadPaymentAccount(account, clientId) complete () match {
                            case Success(s) => s.flatMap(_.userId)
                            case Failure(f) =>
                              log.error(s"Error loading credited account: ${f.getMessage}")
                              None
                          }
                        case None => None
                      }
                    preAuthorizeCard(
                      PreAuthorizationTransaction.defaultInstance
                        .withCardId(cardId)
                        .withAuthorId(userId)
                        .withDebitedAmount(debitedAmount)
                        .withOrderUuid(orderUuid)
                        .withRegisterCard(registerCard)
                        .withPrintReceipt(printReceipt)
                        .copy(
                          ipAddress = ipAddress,
                          browserInfo = browserInfo,
                          creditedUserId = creditedUserId,
                          feesAmount = feesAmount,
                          preRegistrationId = registrationId
                        )
                    ) match {
                      case Some(transaction) =>
                        handleCardPreAuthorization(
                          entityId,
                          orderUuid,
                          replyTo,
                          paymentAccount,
                          registerCard,
                          printReceipt,
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

      case cmd: PreAuthorizeCardCallback => // 3DS
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            loadCardPreAuthorized(orderUuid, preAuthorizationId) match {
              case Some(transaction) =>
                handleCardPreAuthorization(
                  entityId,
                  orderUuid,
                  replyTo,
                  paymentAccount,
                  registerCard,
                  printReceipt,
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
            val clientId = paymentAccount.clientId
              .orElse(cmd.clientId)
              .orElse(
                internalClientId
              )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            paymentAccount.transactions.find(_.id == cardPreAuthorizedTransactionId) match {
              case Some(preAuthorizationTransaction) =>
                val preAuthorizationCanceled =
                  cancelPreAuthorization(orderUuid, cardPreAuthorizedTransactionId)
                val updatedPaymentAccount = paymentAccount.withTransactions(
                  paymentAccount.transactions.filterNot(_.id == cardPreAuthorizedTransactionId) :+
                  preAuthorizationTransaction
                    .withPreAuthorizationCanceled(preAuthorizationCanceled)
                    .copy(clientId = clientId)
                )
                val lastUpdated = now()
                Effect
                  .persist(
                    List(
                      PaymentAccountUpsertedEvent.defaultInstance
                        .withDocument(updatedPaymentAccount)
                        .withLastUpdated(lastUpdated),
                      PreAuthorizationCanceledEvent.defaultInstance
                        .withLastUpdated(lastUpdated)
                        .withOrderUuid(orderUuid)
                        .withDebitedAccount(paymentAccount.externalUuid)
                        .withCardPreAuthorizedTransactionId(cardPreAuthorizedTransactionId)
                        .withPreAuthorizationCanceled(preAuthorizationCanceled)
                    )
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
            val clientId = paymentAccount.clientId
              .orElse(cmd.clientId)
              .orElse(
                internalClientId
              )
            log.info(s"pay in with card pre authorized: $entityId -> ${asJson(cmd)}")
            val maybeTransaction = paymentAccount.transactions
              .filter(t =>
                t.`type` == Transaction.TransactionType.PRE_AUTHORIZATION
              ) //TODO check it
              .find(_.id == preAuthorizationId)
            maybeTransaction match {
              case None =>
                handlePayInWithCardPreauthorizedFailure(
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
                handlePayInWithCardPreauthorizedFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "IllegalPreAuthorizationTransactionStatus"
                )
              case Some(preAuthorizationTransaction)
                  if preAuthorizationTransaction.preAuthorizationCanceled.getOrElse(false) =>
                handlePayInWithCardPreauthorizedFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "PreAuthorizationCanceled"
                )
              case Some(preAuthorizationTransaction)
                  if preAuthorizationTransaction.preAuthorizationValidated.getOrElse(false) =>
                handlePayInWithCardPreauthorizedFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "PreAuthorizationValidated"
                )
              case Some(preAuthorizationTransaction)
                  if preAuthorizationTransaction.preAuthorizationExpired.getOrElse(false) =>
                handlePayInWithCardPreauthorizedFailure(
                  preAuthorizationTransaction.orderUuid,
                  replyTo,
                  "PreAuthorizationExpired"
                )
              case Some(preAuthorizationTransaction)
                  if debitedAmount.getOrElse(
                    preAuthorizationTransaction.amount
                  ) > preAuthorizationTransaction.amount =>
                handlePayInWithCardPreauthorizedFailure(
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
                                  .withCardPreAuthorizedTransactionId(preAuthorizationId)
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
                                    cardPreRegistrationId =
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
                                  registerCard = false,
                                  printReceipt = false,
                                  transaction
                                )
                              case _ =>
                                handlePayInWithCardPreauthorizedFailure(
                                  preAuthorizationTransaction.orderUuid,
                                  replyTo,
                                  "TransactionNotSpecified"
                                )
                            }
                          case _ =>
                            handlePayInWithCardPreauthorizedFailure(
                              preAuthorizationTransaction.orderUuid,
                              replyTo,
                              "CreditedWalletNotFound"
                            )
                        }
                      case _ =>
                        handlePayInWithCardPreauthorizedFailure(
                          preAuthorizationTransaction.orderUuid,
                          replyTo,
                          "CreditedPaymentAccountNotFound"
                        )
                    }
                  case Failure(_) =>
                    handlePayInWithCardPreauthorizedFailure(
                      preAuthorizationTransaction.orderUuid,
                      replyTo,
                      "CreditedPaymentAccountNotFound"
                    )
                }
            }
          case _ =>
            handlePayInWithCardPreauthorizedFailure(
              "",
              replyTo,
              "PaymentAccountNotFound"
            )
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
                    val clientId = paymentAccount.clientId.orElse(
                      internalClientId
                    )
                    val paymentProvider = loadPaymentProvider(clientId)
                    import paymentProvider._
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

    }
  }

  private[this] def handleCardPreAuthorization(
    entityId: String,
    orderUuid: String,
    replyTo: Option[ActorRef[PaymentResult]],
    paymentAccount: PaymentAccount,
    registerCard: Boolean,
    printReceipt: Boolean,
    transaction: Transaction
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
            .copy(clientId = paymentAccount.clientId)
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
          val clientId = paymentAccount.clientId.orElse(Option(softPayClientSettings.clientId))
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
          Effect
            .persist(
              registerCardEvents ++
              List(
                CardPreAuthorizedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withTransactionId(transaction.id)
                  .withCardId(transaction.getCardId)
                  .withDebitedAccount(paymentAccount.externalUuid)
                  .withDebitedAmount(transaction.amount)
                  .withLastUpdated(lastUpdated)
                  .withPrintReceipt(printReceipt)
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
              List(
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

  private[this] def handlePayInWithCardPreauthorizedFailure(
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
