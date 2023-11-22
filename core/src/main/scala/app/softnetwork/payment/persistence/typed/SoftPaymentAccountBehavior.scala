package app.softnetwork.payment.persistence.typed

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.{ActorContext, TimerScheduler}
import akka.persistence.typed.scaladsl.Effect
import app.softnetwork.account.handlers.{DefaultGenerator, Generator}
import app.softnetwork.account.message
import app.softnetwork.account.message.{
  AccountCommand,
  AccountCommandResult,
  AccountCreatedEvent,
  AccountNotFound,
  BasicAccountProfileUpdatedEvent,
  ProfileUpdatedEvent
}
import app.softnetwork.account.model.{BasicAccount, BasicAccountProfile, Principal, PrincipalType}
import app.softnetwork.account.persistence.typed.AccountBehavior
import app.softnetwork.payment.message.AccountMessages
import app.softnetwork.payment.message.AccountMessages.SoftPaymentSignup
import app.softnetwork.payment.message.SoftPaymentAccountEvents.{
  SoftPaymentAccountCreatedEvent,
  SoftPaymentAccountProviderRegisteredEvent
}
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.payment.spi.PaymentProviders
import app.softnetwork.persistence.typed._
import app.softnetwork.scheduler.message.SchedulerEvents.ExternalSchedulerEvent

import java.time.Instant

trait SoftPaymentAccountBehavior extends AccountBehavior[SoftPaymentAccount, BasicAccountProfile] {
  _: Generator =>
  override protected def createAccount(
    entityId: String,
    cmd: message.SignUp
  )(implicit context: ActorContext[AccountCommand]): Option[SoftPaymentAccount] = {
    cmd match {
      case SoftPaymentSignup(_, _, provider, _, _) =>
        PaymentProviders.paymentProvider(provider).client match {
          case Some(client) =>
            SoftPaymentAccount(BasicAccount(cmd, Some(entityId)))
              .map(account =>
                account
                  .withClient(client.withClientApiKey(client.generateApiKey()))
                  .withSecondaryPrincipals(
                    account.secondaryPrincipals.filterNot(
                      _.`type` == PrincipalType.Other
                    ) :+ Principal(PrincipalType.Other, client.clientId)
                  )
              )
          case _ => None
        }
      case _ => SoftPaymentAccount(BasicAccount(cmd, Some(entityId)))
    }
  }

  override protected def createProfileUpdatedEvent(
    uuid: String,
    profile: BasicAccountProfile,
    loginUpdated: Option[Boolean]
  )(implicit context: ActorContext[AccountCommand]): ProfileUpdatedEvent[BasicAccountProfile] =
    BasicAccountProfileUpdatedEvent(uuid, profile, loginUpdated)

  override protected def createAccountCreatedEvent(
    account: SoftPaymentAccount
  )(implicit context: ActorContext[AccountCommand]): AccountCreatedEvent[SoftPaymentAccount] =
    SoftPaymentAccountCreatedEvent(account)

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
    state: Option[SoftPaymentAccount],
    command: AccountCommand,
    replyTo: Option[ActorRef[AccountCommandResult]],
    timers: TimerScheduler[AccountCommand]
  )(implicit
    context: ActorContext[AccountCommand]
  ): Effect[ExternalSchedulerEvent, Option[SoftPaymentAccount]] = {
    command match {
      case AccountMessages.RegisterProvider(provider) =>
        state match {
          case Some(account) =>
            if (account.client.isDefined) {
              Effect.none.thenRun { _ =>
                AccountMessages.ProviderAlreadyRegistered ~> replyTo
              }
            } else {
              PaymentProviders.paymentProvider(provider).client match {
                case Some(client) =>
                  val updatedClient = client.withClientApiKey(client.generateApiKey())
                  Effect
                    .persist(
                      SoftPaymentAccountProviderRegisteredEvent(
                        updatedClient,
                        Instant.now()
                      )
                    )
                    .thenRun(_ => AccountMessages.ProviderRegistered(updatedClient) ~> replyTo)
                case _ =>
                  Effect.none.thenRun { _ =>
                    AccountMessages.ProviderNotRegistered ~> replyTo
                  }
              }
            }
          case _ =>
            Effect.none.thenRun { _ =>
              AccountNotFound ~> replyTo
            }
        }

      case AccountMessages.LoadProvider(clientId) =>
        state match {
          case Some(account) =>
            account.client match {
              case Some(client) =>
                if (client.clientId == clientId) {
                  Effect.none.thenRun { _ =>
                    AccountMessages.ProviderLoaded(client.provider) ~> replyTo
                  }
                } else {
                  Effect.none.thenRun { _ =>
                    AccountMessages.ProviderNotFound ~> replyTo
                  }
                }
              case _ =>
                Effect.none.thenRun { _ =>
                  AccountMessages.ProviderNotFound ~> replyTo
                }
            }
          case _ =>
            Effect.none.thenRun { _ =>
              AccountNotFound ~> replyTo
            }
        }

      case _ => super.handleCommand(entityId, state, command, replyTo, timers)
    }
  }

  /** @param state
    *   - current state
    * @param event
    *   - event to hanlde
    * @return
    *   new state
    */
  override def handleEvent(
    state: Option[SoftPaymentAccount],
    event: ExternalSchedulerEvent
  )(implicit context: ActorContext[_]): Option[SoftPaymentAccount] = {
    event match {
      case SoftPaymentAccountProviderRegisteredEvent(client, lastUpdated) =>
        state.map(_.withClient(client).withLastUpdated(lastUpdated))
      case _ => super.handleEvent(state, event)
    }
  }

}

case object SoftPaymentAccountBehavior extends SoftPaymentAccountBehavior with DefaultGenerator {
  override def persistenceId: String = "SoftPaymentAccount"
}
