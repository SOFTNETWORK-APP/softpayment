package app.softnetwork.payment.service

import app.softnetwork.api.server.HttpCorrelation
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{RecurringPayment, RecurringPaymentView}
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait RecurringPaymentEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  val registerRecurringPayment: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.recurringPaymentRoute)
      .in(jsonBody[RegisterRecurringPayment].description("Recurring payment to register"))
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        oneOf[PaymentResult](
          oneOfVariant[RecurringPaymentRegistered](
            statusCode(StatusCode.Ok).and(
              jsonBody[RecurringPaymentRegistered].description(
                "Recurring payment successfully registered"
              )
            )
          ),
          oneOfVariant[MandateConfirmationRequired](
            statusCode(StatusCode.Ok).and(
              jsonBody[MandateConfirmationRequired].description(
                "Recurring payment registration required a mandate confirmation"
              )
            )
          )
        )
      )
      .serverLogic {
        case (client, session) => { case (cmd, correlationId) =>
          // copy resets the AuditableCommand var, so stamp the cid AFTER the copy
          val updated = cmd.copy(
            debitedAccount = externalUuidWithProfile(session),
            clientId = client.map(_.clientId).orElse(session.clientId)
          )
          updated.withCorrelationId(correlationId) // Story 13.7 — origin stamp
          run(updated).map {
            case r: RecurringPaymentRegistered  => Right(r)
            case r: MandateConfirmationRequired => Right(r)
            case other                          => Left(error(other))
          }
        }
      }
      .description("Register a recurring payment for the authenticated payment account")

  val loadRecurringPayment: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .in(PaymentSettings.PaymentConfig.recurringPaymentRoute)
      .in(path[String])
      .out(
        statusCode(StatusCode.Ok).and(
          jsonBody[RecurringPaymentView]
            .description("Recurring payment successfully loaded")
        )
      )
      .serverLogic(principal =>
        recurringPaymentRegistrationId =>
          run(
            LoadRecurringPayment(
              externalUuidWithProfile(principal._2),
              recurringPaymentRegistrationId
            )
          ).map {
            case r: RecurringPaymentLoaded => Right(r.recurringPayment.view)
            case other                     => Left(error(other))
          }
      )
      .description("Load the recurring payment of the authenticated payment account")

  val updateRecurringCardPaymentRegistration: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.put
      .in(PaymentSettings.PaymentConfig.recurringPaymentRoute)
      .in(
        jsonBody[UpdateRecurringCardPaymentRegistration].description(
          "Recurring card payment update"
        )
      )
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok)
          .and(
            jsonBody[RecurringPayment.RecurringCardPaymentResult]
              .description("Recurring card payment successfully updated")
          )
      )
      .serverLogic(principal => { case (cmd, correlationId) =>
        val updated = cmd.copy(debitedAccount = externalUuidWithProfile(principal._2))
        updated.withCorrelationId(correlationId) // Story 13.7 — origin stamp (after copy)
        run(updated).map {
          case r: RecurringCardPaymentRegistrationUpdated => Right(r.result)
          case other                                      => Left(error(other))
        }
      })
      .description(
        "Update recurring card payment registration of the authenticated payment account"
      )

  val deleteRecurringPayment: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.PaymentConfig.recurringPaymentRoute)
      .in(path[String])
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[RecurringPayment.RecurringCardPaymentResult])
      )
      .serverLogic(principal => { case (recurringPaymentRegistrationId, correlationId) =>
        val cmd =
          UpdateRecurringCardPaymentRegistration(
            externalUuidWithProfile(principal._2),
            recurringPaymentRegistrationId,
            None,
            Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
          )
        cmd.withCorrelationId(correlationId) // Story 13.7 — origin stamp
        run(cmd).map {
          case r: RecurringCardPaymentRegistrationUpdated => Right(r.result)
          case other                                      => Left(error(other))
        }
      })

  val recurringPaymentEndpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      registerRecurringPayment,
      updateRecurringCardPaymentRegistration,
      loadRecurringPayment,
      deleteRecurringPayment
    )
}
