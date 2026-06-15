package app.softnetwork.payment.service

import app.softnetwork.api.server.HttpCorrelation
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.BankAccountView
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait BankAccountEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  val createOrUpdateBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.bankRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .in(jsonBody[BankAccountCommand].description("Bank account to create or update"))
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[BankAccountCreatedOrUpdated].description("Bank account created or updated"))
      )
      .serverLogic {
        case (client, session) => { args =>
          val correlationId = args._1
          val bankAccountCommand = args._2
          import bankAccountCommand._
          val updatedBankAccount =
            if (bankAccount.externalUuid.trim().isEmpty) {
              bankAccount.withExternalUuid(session.id)
            } else {
              bankAccount
            }
          val cmd =
            CreateOrUpdateBankAccount(
              externalUuidWithProfile(session),
              updatedBankAccount,
              clientId = client.map(_.clientId).orElse(session.clientId),
              bankTokenId = bankTokenId
            )
          cmd.withCorrelationId(correlationId) // Story 13.7 — origin stamp
          run(cmd).map {
            case r: BankAccountCreatedOrUpdated => Right(r)
            case other                          => Left(error(other))
          }
        }
      }
      .description("Create or update authenticated user bank account")

  val loadBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.PaymentConfig.bankRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[BankAccountView]
            .description("Authenticated user bank account")
        )
      )
      .serverLogic { case (client, session) =>
        correlationId => {
          val cmd = LoadBankAccount(
            externalUuidWithProfile(session),
            clientId = client.map(_.clientId).orElse(session.clientId)
          )
          cmd.withCorrelationId(correlationId) // Story 13.7 — origin stamp
          run(cmd).map {
            case r: BankAccountLoaded => Right(r.bankAccount.view)
            case other                => Left(error(other))
          }
        }
      }
      .description("Load authenticated user bank account")

  val deleteBankAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.PaymentConfig.bankRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[BankAccountDeleted.type])
      )
      .serverLogic { principal => correlationId =>
        val cmd = DeleteBankAccount(
          externalUuidWithProfile(principal._2),
          Some(false)
        )
        cmd.withCorrelationId(correlationId) // Story 13.7 — origin stamp
        run(cmd).map {
          case BankAccountDeleted => Right(BankAccountDeleted)
          case other              => Left(error(other))
        }
      }
      .description("Delete authenticated user bank account")

  val bankAccountEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      createOrUpdateBankAccount,
      loadBankAccount,
      deleteBankAccount
    )
}
