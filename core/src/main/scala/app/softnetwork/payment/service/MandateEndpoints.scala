package app.softnetwork.payment.service

import app.softnetwork.api.server.HttpCorrelation
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.MandateResult
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait MandateEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  val createMandate: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.mandateRoute)
      .in(jsonBody[Option[IbanMandate]])
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        oneOf[PaymentResult](
          oneOfVariant[MandateCreated.type](
            statusCode(StatusCode.Ok)
              .and(jsonBody[MandateCreated.type].description("Mandate created"))
          ),
          oneOfVariant[MandateConfirmationRequired](
            statusCode(StatusCode.Accepted).and(
              jsonBody[MandateConfirmationRequired].description("Mandate confirmation required")
            )
          )
        )
      )
      .serverLogic { principal => args =>
        val maybeIban = args._1
        val correlationId = args._2
        val cmd =
          CreateMandate(
            externalUuidWithProfile(principal._2),
            iban = maybeIban.map(_.iban),
            clientId = principal._1.map(_.clientId).orElse(principal._2.clientId)
          )
        runCorrelated(cmd, correlationId).map {
          case MandateCreated                 => Right(MandateCreated)
          case r: MandateConfirmationRequired => Right(r)
          case other                          => Left(error(other))
        }
      }
      .description("Create a mandate for the authenticated payment account")

  val cancelMandate: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.delete
      .in(PaymentSettings.PaymentConfig.mandateRoute)
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[MandateCanceled.type].description("Mandate canceled"))
      )
      .serverLogic { case (client, session) =>
        correlationId =>
          val cmd =
            CancelMandate(
              externalUuidWithProfile(session),
              clientId = client.map(_.clientId).orElse(session.clientId)
            )
          runCorrelated(cmd, correlationId).map {
            case MandateCanceled => Right(MandateCanceled)
            case other           => Left(error(other))
          }
      }
      .description("Create Mandate for the authenticated payment account")

  val updateMandateStatus: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.PaymentConfig.mandateRoute)
      .get
      .in(query[String]("MandateId").description("Mandate Id"))
      .in(HttpCorrelation.correlationInput) // Story 13.7 — origin correlation id
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[MandateResult])
      )
      .description("Update mandate status web hook")
      .serverLogic { case (mandateId, cid) =>
        val cmd = UpdateMandateStatus(mandateId)
        runCorrelated(cmd, cid).map {
          case r: MandateStatusUpdated => Right(r.result)
          case other                   => Left(error(other))
        }
      }

  val mandateEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      createMandate,
      updateMandateStatus,
      cancelMandate
    )
}
