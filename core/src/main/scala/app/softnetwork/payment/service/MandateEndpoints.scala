package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.MandateResult
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait MandateEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val createMandate: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.post
      .in(PaymentSettings.MandateRoute)
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
      .serverLogic(principal =>
        _ =>
          run(CreateMandate(externalUuidWithProfile(principal._2))).map {
            case MandateCreated =>
              Right((principal._1._1, principal._1._2, MandateCreated))
            case r: MandateConfirmationRequired =>
              Right((principal._1._1, principal._1._2, r))
            case other => Left(error(other))
          }
      )
      .description("Create a mandate for the authenticated payment account")

  val cancelMandate: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.delete
      .in(PaymentSettings.MandateRoute)
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[MandateCanceled.type].description("Mandate canceled"))
      )
      .serverLogic(principal =>
        _ =>
          run(CancelMandate(externalUuidWithProfile(principal._2))).map {
            case MandateCanceled =>
              Right((principal._1._1, principal._1._2, MandateCanceled))
            case other => Left(error(other))
          }
      )
      .description("Create Mandate for the authenticated payment account")

  val updateMandateStatus: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.MandateRoute)
      .get
      .in(query[String]("MandateId").description("Mandate Id"))
      .out(
        statusCode(StatusCode.Ok).and(jsonBody[MandateResult])
      )
      .description("Update mandate status web hook")
      .serverLogic(mandateId =>
        run(UpdateMandateStatus(mandateId)).map {
          case r: MandateStatusUpdated => Right(r.result)
          case other                   => Left(error(other))
        }
      )

  val mandateEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      createMandate,
      updateMandateStatus,
      cancelMandate
    )
}
