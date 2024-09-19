package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentEvents.{
  PaymentAccountUpsertedEvent,
  PaymentMethodRegisteredEvent,
  WalletRegisteredEvent
}
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.message.TransactionEvents.{
  PreAuthorizationCanceledEvent,
  PreAuthorizationFailedEvent,
  PreAuthorizedEvent
}
import app.softnetwork.payment.model.{
  Card,
  PaymentAccount,
  Paypal,
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

trait PreAuthorizationCommandHandler
    extends EntityCommandHandler[
      PreAuthorizationCommand,
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
    command: PreAuthorizationCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[PreAuthorizationCommand]
  )(implicit
    context: ActorContext[PreAuthorizationCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)

    command match {
      case cmd: PreAuthorize =>
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
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            paymentAccount.userId match {
              case Some(userId) =>
                var paymentType = cmd.paymentType
                val paymentMethodId =
                  registrationId match {
                    case Some(id) =>
                      registerPaymentMethod(id, registrationData)
                    case _ =>
                      cmd.paymentMethodId match {
                        case Some(paymentMethodId) =>
                          paymentAccount.paymentMethods
                            .find(paymentMethod => paymentMethod.id == paymentMethodId) match {
                            case Some(paymentMethod) if paymentMethod.enabled =>
                              paymentType = paymentMethod.paymentType
                              Some(paymentMethodId)
                            case Some(_) =>
                              log.warn(s"Payment method $paymentMethodId selected is disabled")
                              None
                            case _ =>
                              None
                          }
                        case _ => None
                      }
                  }
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
                val registerMeansOfPayment: Boolean =
                  cmd.registerMeansOfPayment.getOrElse(cmd.paymentType.isCard && cmd.registerCard)
                preAuthorize(
                  PreAuthorizationTransaction.defaultInstance
                    .withAuthorId(userId)
                    .withDebitedAmount(debitedAmount)
                    .withOrderUuid(orderUuid)
                    .withRegisterMeansOfPayment(registerMeansOfPayment)
                    .withPrintReceipt(printReceipt)
                    .copy(
                      paymentMethodId = paymentMethodId,
                      ipAddress = ipAddress,
                      browserInfo = browserInfo,
                      creditedUserId = creditedUserId,
                      feesAmount = feesAmount,
                      preRegistrationId = registrationId,
                      paymentType = paymentType
                    )
                ) match {
                  case Some(transaction) =>
                    handlePreAuthorization(
                      entityId,
                      orderUuid,
                      replyTo,
                      paymentAccount,
                      registerMeansOfPayment,
                      printReceipt,
                      transaction,
                      registerWallet
                    )
                  case _ => // pre authorization failed
                    Effect.none.thenRun(_ => PaymentNotPreAuthorized ~> replyTo)
                }
              case _ => // no userId
                Effect.none.thenRun(_ => PaymentNotPreAuthorized ~> replyTo)
            }
          case _ => // no payment account
            Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: PreAuthorizeCallback => // 3DS
        import cmd._
        state match {
          case Some(paymentAccount) =>
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            loadPreAuthorization(orderUuid, preAuthorizationId) match {
              case Some(transaction) =>
                handlePreAuthorization(
                  entityId,
                  orderUuid,
                  replyTo,
                  paymentAccount,
                  registerMeansOfPayment,
                  printReceipt,
                  transaction
                )
              case _ => Effect.none.thenRun(_ => PaymentNotPreAuthorized ~> replyTo)
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
            paymentAccount.transactions.find(_.id == preAuthorizationId) match {
              case Some(preAuthorizationTransaction) =>
                val preAuthorizationCanceled =
                  cancelPreAuthorization(orderUuid, preAuthorizationId)
                val updatedPaymentAccount = paymentAccount.withTransactions(
                  paymentAccount.transactions.filterNot(_.id == preAuthorizationId) :+
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
                        .withPreAuthorizedTransactionId(preAuthorizationId)
                        .withPreAuthorizationCanceled(preAuthorizationCanceled)
                    )
                  )
                  .thenRun(_ => PreAuthorizationCanceled(preAuthorizationCanceled) ~> replyTo)
              case _ => // should never be the case
                Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

    }
  }

  private[this] def handlePreAuthorization(
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
    var updatedPaymentAccount =
      paymentAccount
        .withTransactions(
          paymentAccount.transactions
            .filterNot(_.id == transaction.id) :+ transaction
            .copy(clientId = paymentAccount.clientId)
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
          val registerPaymentMethodEvents: List[ExternalSchedulerEvent] =
            if (registerMeansOfPayment) {
              transaction.paymentMethodId match {
                case Some(paymentMethodId) =>
                  loadPaymentMethod(paymentMethodId) match {
                    case Some(paymentMethod) =>
                      transaction.preRegistrationId match {
                        case None =>
                          // register means of payment pre authorized without pre registration
                          attachPaymentMethod(paymentMethodId, transaction.authorId)
                        case _ =>
                      }
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
          Effect
            .persist(
              registerPaymentMethodEvents ++
              List(
                PreAuthorizedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withTransactionId(transaction.id)
                  .withPaymentMethodId(transaction.paymentMethodId.getOrElse(""))
                  .withDebitedAccount(paymentAccount.externalUuid)
                  .withDebitedAmount(transaction.amount)
                  .withLastUpdated(lastUpdated)
                  .withPrintReceipt(printReceipt)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
            )
            .thenRun(_ => PaymentPreAuthorized(transaction.id) ~> replyTo)
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
                PreAuthorizationFailedEvent.defaultInstance
                  .withOrderUuid(orderUuid)
                  .withResultMessage(transaction.resultMessage)
                  .withTransaction(transaction)
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(updatedPaymentAccount)
                .withLastUpdated(lastUpdated)
            )
            .thenRun(_ => PreAuthorizationFailed(transaction.resultMessage) ~> replyTo)
        }
    }
  }

}
