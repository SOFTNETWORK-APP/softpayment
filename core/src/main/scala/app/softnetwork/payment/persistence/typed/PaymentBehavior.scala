package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.cluster.sharding.typed.ShardingEnvelope
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.config.PaymentSettings.PaymentConfig.akkaNodeRole
import app.softnetwork.payment.handlers.{PaymentDao, SoftPayAccountDao}
import app.softnetwork.payment.message.PaymentEvents._
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.message.TransactionEvents._
import app.softnetwork.payment.model.LegalUser.LegalUserType
import app.softnetwork.payment.model.NaturalUser.NaturalUserType
import app.softnetwork.payment.model._
import app.softnetwork.persistence._
import app.softnetwork.persistence.message.{BroadcastEvent, CrudEvent}
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.config.SchedulerSettings
import app.softnetwork.scheduler.message.SchedulerEvents.{
  ExternalSchedulerEvent,
  SchedulerEventWithCommand
}
import app.softnetwork.serialization.asJson
import app.softnetwork.time._
import org.slf4j.Logger

import java.time.LocalDate
import java.util.Date
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/** Created by smanciot on 22/04/2022.
  */
trait PaymentBehavior
    extends TimeStampedBehavior[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]
    with ManifestWrapper[PaymentAccount]
    with PaymentCommandHandler
    with PaymentTimers { self =>

  override protected val manifestWrapper: ManifestW = ManifestW()

  def paymentDao: PaymentDao = PaymentDao

  def softPayAccountDao: SoftPayAccountDao = SoftPayAccountDao

  /** @return
    *   node role required to start this actor
    */
  override lazy val role: String = akkaNodeRole

  override def init(system: ActorSystem[_], maybeRole: Option[String] = None)(implicit
    c: ClassTag[PaymentCommand]
  ): ActorRef[ShardingEnvelope[PaymentCommand]] = {
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

  private[this] lazy val cardCommandHandler: PartialFunction[PaymentCommand, EntityCommandHandler[
    PaymentCommand,
    PaymentAccount,
    ExternalSchedulerEvent,
    PaymentResult
  ]] = { case _: CardCommand =>
    new CardCommandHandler {
      override def paymentDao: PaymentDao = self.paymentDao
      override def softPayAccountDao: SoftPayAccountDao = self.softPayAccountDao
    }.asInstanceOf[EntityCommandHandler[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]]
  }

  private[this] lazy val payInCommandHandler: PartialFunction[PaymentCommand, EntityCommandHandler[
    PaymentCommand,
    PaymentAccount,
    ExternalSchedulerEvent,
    PaymentResult
  ]] = { case _: PayInCommand =>
    new PayInCommandHandler {
      override def paymentDao: PaymentDao = self.paymentDao
      override def softPayAccountDao: SoftPayAccountDao = self.softPayAccountDao
    }.asInstanceOf[EntityCommandHandler[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]]
  }

  private[this] lazy val payOutCommandHandler: PartialFunction[PaymentCommand, EntityCommandHandler[
    PaymentCommand,
    PaymentAccount,
    ExternalSchedulerEvent,
    PaymentResult
  ]] = { case _: PayOutCommand =>
    new PayOutCommandHandler {
      override def paymentDao: PaymentDao = self.paymentDao
      override def softPayAccountDao: SoftPayAccountDao = self.softPayAccountDao
    }.asInstanceOf[EntityCommandHandler[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]]
  }

  private[this] lazy val recurringPaymentCommandHandler
    : PartialFunction[PaymentCommand, EntityCommandHandler[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]] = { case _: RecurringPaymentCommand =>
    new RecurringPaymentCommandHandler {
      override def paymentDao: PaymentDao = self.paymentDao
      override def softPayAccountDao: SoftPayAccountDao = self.softPayAccountDao
      override def persistenceId: String = self.persistenceId
    }.asInstanceOf[EntityCommandHandler[
      PaymentCommand,
      PaymentAccount,
      ExternalSchedulerEvent,
      PaymentResult
    ]]
  }

  override def entityCommandHandler: PartialFunction[PaymentCommand, EntityCommandHandler[
    PaymentCommand,
    PaymentAccount,
    ExternalSchedulerEvent,
    PaymentResult
  ]] = {
    cardCommandHandler orElse payInCommandHandler orElse payOutCommandHandler orElse recurringPaymentCommandHandler orElse super.entityCommandHandler
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
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)
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
            List(
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

      case cmd: Refund =>
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
            (paymentAccount.transactions.find(_.id == payInTransactionId) match {
              case None => loadPayInTransaction(orderUuid, payInTransactionId, None)
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
                        .copy(
                          feesRefundAmount = feesRefundAmount
                        )
                    )
                  ) match {
                    case None => Effect.none.thenRun(_ => TransactionNotFound ~> replyTo)
                    case Some(transaction) =>
                      keyValueDao.addKeyValue(transaction.id, entityId)
                      val lastUpdated = now()
                      val updatedPaymentAccount = paymentAccount
                        .withTransactions(
                          paymentAccount.transactions.filterNot(_.id == transaction.id)
                          :+ transaction.copy(clientId = clientId)
                        )
                        .withLastUpdated(lastUpdated)
                      val paymentAccountUpsertedEvent =
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
                              List(
                                RefundFailedEvent.defaultInstance
                                  .withOrderUuid(orderUuid)
                                  .withResultMessage(transaction.resultMessage)
                                  .withTransaction(transaction)
                              ) :+ paymentAccountUpsertedEvent
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
                                List(
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
                                ) :+ paymentAccountUpsertedEvent
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
                                List(
                                  RefundFailedEvent.defaultInstance
                                    .withOrderUuid(orderUuid)
                                    .withResultMessage(transaction.resultMessage)
                                    .withTransaction(transaction)
                                ) :+ paymentAccountUpsertedEvent
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
                          List(
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

      case cmd: Transfer =>
        import cmd._
        state match {
          case Some(paymentAccount) => // debited account
            val clientId = paymentAccount.clientId
              .orElse(cmd.clientId)
              .orElse(
                internalClientId
              )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
            var maybeCreditedPaymentAccount: Option[PaymentAccount] = None
            transfer(paymentAccount.userId match {
              case Some(authorId) =>
                paymentAccount.walletId match {
                  case Some(debitedWalletId) =>
                    // load credited payment account
                    paymentDao.loadPaymentAccount(creditedAccount, clientId) complete () match {
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
                    :+ transaction.copy(clientId = clientId)
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
                        feesAmount = 0, /* fees have already been applied with Transfer */
                        currency,
                        externalReference
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
                      List(
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
                      List(
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
                // check if a mandate is already associated to this bank account and activated
                if (paymentAccount.mandateActivated) {
                  Effect.none.thenRun(_ => MandateAlreadyExists ~> replyTo)
                } else if (paymentAccount.documents.exists(!_.status.isKycDocumentValidated)) {
                  Effect.none.thenRun(_ => MandateNotCreated ~> replyTo)
                } else {
                  addMandate(
                    entityId,
                    replyTo,
                    debitedAccount,
                    paymentAccount,
                    creditedUserId,
                    paymentAccount.bankAccount.flatMap(_.id),
                    iban,
                    clientId
                  )
                }
              case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: CancelMandate =>
        state match {
          case Some(paymentAccount) =>
            if (paymentAccount.mandateExists && paymentAccount.mandateRequired) {
              Effect
                .persist(
                  List(
                    MandateCancelationFailedEvent(paymentAccount.externalUuid, now())
                  )
                )
                .thenRun(_ => MandateNotCanceled ~> replyTo)
            } else {
              val clientId = paymentAccount.clientId
                .orElse(cmd.clientId)
                .orElse(
                  internalClientId
                )
              val paymentProvider = loadPaymentProvider(clientId)
              import paymentProvider._
              paymentAccount.mandate match {
                case Some(value) =>
                  cancelMandate(value.id) match {
                    case Some(_) =>
                      keyValueDao.removeKeyValue(value.id)
                      val lastUpdated = now()
                      val updatePaymentAccount = paymentAccount
                        .copy(
                          bankAccount = paymentAccount.bankAccount.map(
                            _.copy(
                              mandateId = None,
                              mandateStatus = None
                            )
                          ),
                          mandates = paymentAccount.mandates
                            .filterNot(_.id == value.id)
                        )
                        .withLastUpdated(lastUpdated)
                      Effect
                        .persist(
                          List(
                            MandateUpdatedEvent.defaultInstance
                              .withExternalUuid(paymentAccount.externalUuid)
                              .withLastUpdated(lastUpdated)
                              .withBankAccountId(
                                paymentAccount.bankAccount.flatMap(_.id).getOrElse("")
                              )
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
            }
          case _ => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
        }

      case cmd: UpdateMandateStatus =>
        import cmd._
        state match {
          case Some(paymentAccount) =>
            paymentAccount.userId match {
              case Some(userId) =>
                val bankAccountId = paymentAccount.bankAccount.flatMap(_.id)
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
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
                        mandates = paymentAccount.mandates
                          .filterNot(_.id == mandateId) :+ paymentAccount.mandates
                          .find(_.id == mandateId)
                          .getOrElse(
                            Mandate.defaultInstance
                              .withId(mandateId)
                              .withCreatedDate(lastUpdated)
                          )
                          .withMandateStatus(internalStatus)
                          .copy(
                            resultCode = report.resultCode,
                            resultMessage = report.resultMessage
                          )
                      )
                      .withLastUpdated(lastUpdated)
                    Effect
                      .persist(
                        List(
                          MandateUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withMandateId(mandateId)
                            .withMandateStatus(internalStatus)
                            .withBankAccountId(bankAccountId.getOrElse(""))
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
                    paymentAccount.mandate match {
                      case Some(value) =>
                        if (paymentAccount.mandateActivated) {
                          val clientId = paymentAccount.clientId
                            .orElse(cmd.clientId)
                            .orElse(
                              internalClientId
                            )
                          val paymentProvider = loadPaymentProvider(clientId)
                          import paymentProvider._
                          directDebit(
                            Some(
                              DirectDebitTransaction.defaultInstance
                                .withAuthorId(creditedUserId)
                                .withCreditedUserId(creditedUserId)
                                .withCreditedWalletId(creditedWalletId)
                                .withDebitedAmount(debitedAmount)
                                .withFeesAmount(feesAmount)
                                .withCurrency(currency)
                                .withMandateId(value.id)
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
                                  :+ transaction.copy(clientId = clientId)
                                )
                                .withLastUpdated(lastUpdated)
                              if (
                                transaction.status.isTransactionSucceeded || transaction.status.isTransactionCreated
                              ) {
                                Effect
                                  .persist(
                                    List(
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
                                    List(
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
                    val clientId = paymentAccount.clientId
                      .orElse(cmd.clientId)
                      .orElse(
                        internalClientId
                      )
                    val paymentProvider = loadPaymentProvider(clientId)
                    import paymentProvider._
                    val transactionDate: LocalDate = Date.from(transaction.createdDate)
                    loadDirectDebitTransaction(
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
                            :+ updatedTransaction.copy(clientId = clientId)
                          )
                          .withLastUpdated(lastUpdated)
                        if (t.status.isTransactionSucceeded || t.status.isTransactionCreated) {
                          Effect
                            .persist(
                              List(
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
                              List(
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

      case cmd: TriggerSchedule4Payment =>
        import cmd.schedule._
        if (key.startsWith(nextRecurringPayment)) {
          state match {
            case Some(paymentAccount) =>
              val recurringPaymentRegistrationId = key.split("#").last
              Effect.none.thenRun(_ => {
                context.self ! ExecuteNextRecurringPayment(
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
                  loadPaymentAccount(entityId, None, user, cmd.clientId)
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
                            .withNaturalUserType(
                              updatedLegalUser.legalRepresentative.naturalUserType
                                .getOrElse(NaturalUserType.COLLECTOR)
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
                            updatedLegalUser.legalRepresentative.withNaturalUserType(
                              updatedLegalUser.legalRepresentative.naturalUserType.getOrElse(
                                NaturalUserType.COLLECTOR
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
                          .withNaturalUserType(
                            updatedNaturalUser.naturalUserType.getOrElse(NaturalUserType.COLLECTOR)
                          )
                      )
                    } else if (updatedUser.isNaturalUser) {
                      val updatedNaturalUser = updatedUser.naturalUser.get
                      PaymentAccount.User.NaturalUser(
                        updatedNaturalUser.withNaturalUserType(
                          updatedNaturalUser.naturalUserType.getOrElse(NaturalUserType.COLLECTOR)
                        )
                      )
                    } else {
                      updatedUser
                    }
                }

              val lastUpdated = now()

              val shouldUpdateIban =
                !paymentAccount.bankAccount.exists(_.checkIfSameIban(bankAccount.iban))

              val iban = bankAccount.iban

              val shouldUpdateBic =
                shouldUpdateIban || !paymentAccount.bankAccount.exists(
                  _.checkIfSameBic(bankAccount.bic)
                )

              val bic = bankAccount.bic

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

                  val clientId = paymentAccount.clientId.orElse(
                    internalClientId
                  )
                  val paymentProvider = loadPaymentProvider(clientId)
                  import paymentProvider._
                  (paymentAccount.userId match {
                    case None =>
                      createOrUpdatePaymentAccount(
                        Some(updatedPaymentAccount),
                        acceptedTermsOfPSP.getOrElse(false),
                        ipAddress,
                        userAgent
                      )
                    case Some(_) if shouldUpdateUser =>
                      if (shouldUpdateUserType) {
                        createOrUpdatePaymentAccount(
                          Some(updatedPaymentAccount.resetUserId(None)),
                          acceptedTermsOfPSP.getOrElse(false),
                          ipAddress,
                          userAgent
                        )
                      } else {
                        createOrUpdatePaymentAccount(
                          Some(updatedPaymentAccount),
                          acceptedTermsOfPSP.getOrElse(false),
                          ipAddress,
                          userAgent
                        )
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
                                List(
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
                                  List(
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
                                      List(
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
                                  List(
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
                                  List(
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
                                List(
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

      case cmd: CreateOrUpdateKycDocument =>
        import cmd._
        state match {
          case Some(paymentAccount) if paymentAccount.hasAcceptedTermsOfPSP =>
            val documentId = kycDocument.id.getOrElse("")
            paymentAccount.documents.find(_.`type` == kycDocument.`type`).flatMap(_.id) match {
              case Some(previous) if previous != documentId =>
                keyValueDao.removeKeyValue(previous)
              case _ =>
            }
            keyValueDao.addKeyValue(documentId, entityId)

            val lastUpdated = now()

            val updatedDocument =
              paymentAccount.documents
                .find(_.`type` == kycDocument.`type`)
                .getOrElse(
                  KycDocument.defaultInstance
                    .withCreatedDate(lastUpdated)
                    .withType(kycDocument.`type`)
                )
                .withLastUpdated(lastUpdated)
                .withId(documentId)
                .withStatus(kycDocument.status)
                .copy(
                  refusedReasonType = kycDocument.refusedReasonType,
                  refusedReasonMessage = kycDocument.refusedReasonMessage
                )

            val newDocuments =
              paymentAccount.documents.filterNot(_.`type` == kycDocument.`type`) :+
              updatedDocument

            Effect
              .persist(
                List(
                  DocumentsUpdatedEvent.defaultInstance
                    .withExternalUuid(paymentAccount.externalUuid)
                    .withLastUpdated(lastUpdated)
                    .withDocuments(newDocuments)
                ) ++ List(
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
              .thenRun(_ => KycDocumentCreatedOrUpdated ~> replyTo)
          case None => Effect.none.thenRun(_ => PaymentAccountNotFound ~> replyTo)
          case _    => Effect.none.thenRun(_ => AcceptedTermsOfPSPRequired ~> replyTo)
        }

      case cmd: AddKycDocument =>
        import cmd._
        state match {
          case Some(paymentAccount) if paymentAccount.hasAcceptedTermsOfPSP =>
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
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
                        List(
                          DocumentsUpdatedEvent.defaultInstance
                            .withExternalUuid(paymentAccount.externalUuid)
                            .withLastUpdated(lastUpdated)
                            .withDocuments(newDocuments)
                        ) ++ List(
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
            val clientId = paymentAccount.clientId.orElse(
              internalClientId
            )
            val paymentProvider = loadPaymentProvider(clientId)
            import paymentProvider._
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
                    List(
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

      case cmd: ValidateUboDeclaration =>
        state match {
          case Some(paymentAccount) =>
            import cmd._
            paymentAccount.getLegalUser.uboDeclaration match {
              case None => Effect.none.thenRun(_ => UboDeclarationNotFound ~> replyTo)
              case Some(uboDeclaration)
                  if uboDeclaration.status.isUboDeclarationCreated ||
                    uboDeclaration.status.isUboDeclarationIncomplete || uboDeclaration.status.isUboDeclarationRefused =>
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
                validateDeclaration(
                  paymentAccount.userId.getOrElse(""),
                  uboDeclaration.id,
                  ipAddress,
                  userAgent
                ) match {
                  case Some(declaration) =>
                    val updatedUbo = declaration.withUbos(uboDeclaration.ubos)
                    val lastUpdated = now()
                    Effect
                      .persist(
                        List(
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
                val clientId = paymentAccount.clientId.orElse(
                  internalClientId
                )
                val paymentProvider = loadPaymentProvider(clientId)
                import paymentProvider._
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
                      List(
                        UboDeclarationUpdatedEvent.defaultInstance
                          .withExternalUuid(paymentAccount.externalUuid)
                          .withLastUpdated(lastUpdated)
                          .withUboDeclaration(updatedDeclaration)
                      )
                    if (
                      internalStatus.isUboDeclarationIncomplete || internalStatus.isUboDeclarationRefused
                    ) {
                      events = events ++
                        List(
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
                        List(
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
              List(
                PaymentAccountStatusUpdatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.COMPTE_OK)
              ) ++
              List(
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
                  List(
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
                List(
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

            val events: List[ExternalSchedulerEvent] =
              List(
                PaymentAccountStatusUpdatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
              ) ++
              List(
                RegularUserInvalidatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withUserId(userId)
              )

            val updatedPaymentAccount = paymentAccount
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
              if PaymentSettings.PaymentConfig.disableBankAccountDeletion && !cmd.force.getOrElse(
                false
              ) =>
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
                    List(
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
                        List(
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
                    List(
                      DocumentsUpdatedEvent.defaultInstance
                        .withExternalUuid(paymentAccount.externalUuid)
                        .withLastUpdated(lastUpdated)
                        .withDocuments(updatedPaymentAccount.documents)
                    ) ++
                    List(
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

      case _ => super.handleCommand(entityId, state, command, replyTo, timers)
    }
  }

  private def addMandate(
    entityId: String,
    replyTo: Option[ActorRef[PaymentResult]],
    creditedAccount: String,
    paymentAccount: PaymentAccount,
    creditedUserId: String,
    bankAccountId: Option[String],
    iban: Option[String] = None,
    clientId: Option[String] = None
  )(implicit
    context: ActorContext[_],
    softPayClientSettings: SoftPayClientSettings
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    val _clientId =
      paymentAccount.clientId.orElse(clientId).orElse(Option(softPayClientSettings.clientId))
    val paymentProvider = loadPaymentProvider(_clientId)
    import paymentProvider._
    mandate(creditedAccount, creditedUserId, bankAccountId, iban) match {
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
              List(
                MandateUpdatedEvent.defaultInstance
                  .withExternalUuid(paymentAccount.externalUuid)
                  .withLastUpdated(lastUpdated)
                  .withMandateId(mandateResult.id)
                  .withMandateStatus(mandateResult.status)
                  .withBankAccountId(bankAccountId.getOrElse(""))
              ) :+
              PaymentAccountUpsertedEvent.defaultInstance
                .withLastUpdated(lastUpdated)
                .withDocument(updatePaymentAccount)
            )
            .thenRun(_ =>
              (mandateResult.status match {
                case Mandate.MandateStatus.MANDATE_CREATED =>
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

  private[this] def updateDocumentStatus(
    paymentAccount: PaymentAccount,
    document: KycDocument,
    documentId: String,
    maybeStatus: Option[KycDocument.KycDocumentStatus] = None
  )(implicit
    system: ActorSystem[_],
    softPayClientSettings: SoftPayClientSettings
  ): (KycDocumentValidationReport, List[ExternalSchedulerEvent]) = {
    var events: List[ExternalSchedulerEvent] = List.empty
    val lastUpdated = now()

    val userId = paymentAccount.userId.getOrElse("")

    val clientId = paymentAccount.clientId.orElse(Option(softPayClientSettings.clientId))
    val paymentProvider = loadPaymentProvider(clientId)
    import paymentProvider._
    val report = loadDocumentStatus(userId, documentId, document.`type`)

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
      List(
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
      List(
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
          List(
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
        List(
          PaymentAccountStatusUpdatedEvent.defaultInstance
            .withExternalUuid(paymentAccount.externalUuid)
            .withLastUpdated(lastUpdated)
            .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
        )
      updatedPaymentAccount = updatedPaymentAccount
        .withPaymentAccountStatus(PaymentAccount.PaymentAccountStatus.DOCUMENTS_KO)
    } else if (internalStatus.isKycDocumentOutOfDate && !paymentAccount.documentOutdated) {
      events = events ++
        List(
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

object PaymentBehavior extends PaymentBehavior
