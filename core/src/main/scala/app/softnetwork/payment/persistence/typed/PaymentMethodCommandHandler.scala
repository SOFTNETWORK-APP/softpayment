package app.softnetwork.payment.persistence.typed

import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.api.config.SoftPayClientSettings
import app.softnetwork.payment.message.PaymentEvents.{
  PaymentAccountUpsertedEvent,
  PaymentMethodPreRegisteredEvent,
  WalletRegisteredEvent
}
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{Card, CardOwner, PaymentAccount, Paypal, Transaction}
import app.softnetwork.persistence.now
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent
import app.softnetwork.time._
import org.slf4j.Logger

trait PaymentMethodCommandHandler
    extends EntityCommandHandler[
      PaymentMethodCommand,
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
    command: PaymentMethodCommand,
    replyTo: Option[ActorRef[PaymentResult]],
    timers: TimerScheduler[PaymentMethodCommand]
  )(implicit
    context: ActorContext[PaymentMethodCommand]
  ): Effect[ExternalSchedulerEvent, Option[PaymentAccount]] = {
    implicit val system: ActorSystem[_] = context.system
    implicit val log: Logger = context.log
    implicit val softPayClientSettings: SoftPayClientSettings = SoftPayClientSettings(system)
    val internalClientId = Option(softPayClientSettings.clientId)

    command match {
      case cmd: PreRegisterPaymentMethod =>
        import cmd._
        val (pa, registerWallet) = createOrUpdateCustomer(entityId, state, user, currency, clientId)
        pa match {
          case Some(paymentAccount) =>
            val paymentAccountUpsertedEvent =
              PaymentAccountUpsertedEvent.defaultInstance
                .withDocument(
                  paymentAccount
                )
            paymentAccount.userId match {
              case Some(userId) =>
                paymentAccount.walletId match {
                  case Some(walletId) =>
                    val clientId = paymentAccount.clientId
                      .orElse(cmd.clientId)
                      .orElse(
                        internalClientId
                      )
                    val paymentProvider = loadPaymentProvider(clientId)
                    import paymentProvider._
                    val lastUpdated = now()
                    preRegisterPaymentMethod(
                      Option(userId),
                      currency,
                      user.externalUuid,
                      paymentType
                    ) match {
                      case Some(preRegistration) =>
                        keyValueDao.addKeyValue(preRegistration.id, entityId)
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
                              PaymentMethodPreRegisteredEvent.defaultInstance
                                .withOrderUuid(orderUuid)
                                .withLastUpdated(lastUpdated)
                                .withExternalUuid(user.externalUuid)
                                .withUserId(userId)
                                .withWalletId(walletId)
                                .withPreRegistrationId(preRegistration.id)
                                .withPaymentType(paymentType)
                                .copy(
                                  owner = paymentType match {
                                    case Transaction.PaymentType.CARD =>
                                      Option(
                                        CardOwner.defaultInstance
                                          .withFirstName(user.firstName)
                                          .withLastName(user.lastName)
                                          .withBirthday(user.birthday)
                                      )
                                    case _ => None
                                  }
                                )
                            ) ++ walletEvents :+ paymentAccountUpsertedEvent
                          )
                          .thenRun(_ => PaymentMethodPreRegistered(preRegistration) ~> replyTo)
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
                            .thenRun(_ => PaymentMethodNotPreRegistered ~> replyTo)
                        } else {
                          Effect
                            .persist(
                              paymentAccountUpsertedEvent
                            )
                            .thenRun(_ => PaymentMethodNotPreRegistered ~> replyTo)
                        }
                    }

                  case _ =>
                    Effect
                      .persist(
                        paymentAccountUpsertedEvent
                      )
                      .thenRun(_ => PaymentMethodNotPreRegistered ~> replyTo)
                }

              case _ =>
                Effect
                  .persist(
                    paymentAccountUpsertedEvent
                  )
                  .thenRun(_ => PaymentMethodNotPreRegistered ~> replyTo)
            }

          case _ => Effect.none.thenRun(_ => PaymentMethodNotPreRegistered ~> replyTo)
        }

      case _: LoadPaymentMethods =>
        state match {
          case Some(paymentAccount) =>
            Effect.none.thenRun(_ => PaymentMethodsLoaded(paymentAccount.paymentMethods) ~> replyTo)
          case _ => Effect.none.thenRun(_ => PaymentMethodsNotLoaded ~> replyTo)
        }

      case cmd: DisablePaymentMethod =>
        state match {
          case Some(paymentAccount) =>
            paymentAccount.paymentMethods.find(_.id == cmd.paymentMethodId) match {
              case Some(paymentMethod) if paymentMethod.enabled =>
                paymentAccount.recurryingPayments.find(r =>
                  r.`type`.isCard && r.getCardId == cmd.paymentMethodId &&
                  r.nextPaymentDate.isDefined
                ) match {
                  case Some(_) =>
                    Effect.none.thenRun(_ => PaymentMethodNotDisabled ~> replyTo)
                  case _ =>
                    val clientId = paymentAccount.clientId.orElse(
                      internalClientId
                    )
                    val paymentProvider = loadPaymentProvider(clientId)
                    import paymentProvider._
                    disablePaymentMethod(cmd.paymentMethodId) match {
                      case Some(_) =>
                        val lastUpdated = now()
                        val updatedPaymentAccount =
                          paymentMethod match {
                            case card: Card =>
                              paymentAccount
                                .withCards(
                                  paymentAccount.cards.filterNot(
                                    _.id == cmd.paymentMethodId
                                  ) :+ card.withActive(false)
                                )
                            case paypal: Paypal =>
                              paymentAccount
                                .withPaypals(
                                  paymentAccount.paypals.filterNot(
                                    _.id == cmd.paymentMethodId
                                  ) :+ paypal.withActive(false)
                                )
                            case _ => paymentAccount
                          }
                        Effect
                          .persist(
                            PaymentAccountUpsertedEvent.defaultInstance
                              .withDocument(
                                updatedPaymentAccount
                                  .withLastUpdated(lastUpdated)
                              )
                          )
                          .thenRun(_ => PaymentMethodDisabled ~> replyTo)
                      case _ => Effect.none.thenRun(_ => PaymentMethodNotDisabled ~> replyTo)
                    }
                }
              case _ => Effect.none.thenRun(_ => PaymentMethodNotDisabled ~> replyTo)
            }
          case _ => Effect.none.thenRun(_ => PaymentMethodNotDisabled ~> replyTo)
        }

    }
  }

}
