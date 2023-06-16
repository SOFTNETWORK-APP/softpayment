package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.MandateResult
import org.softnetwork.session.model.Session
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.{Method, StatusCode}
import sttp.model.headers.CookieValueWithMeta
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait MandateEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val createMandate: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
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
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[PaymentErrorMessage].description("Mandate creation failure"))
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[PaymentAccountNotFound.type].description("Payment account not found"))
          )
        )
      )
      .serverLogic(principal =>
        _ =>
          run(CreateMandate(externalUuidWithProfile(principal._2))).map {
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
            case other => Right((principal._1._1, principal._1._2, other))
          }
      )
      .description("Create a mandate for the authenticated payment account")

  val cancelMandate: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
    secureEndpoint.delete
      .in(PaymentSettings.MandateRoute)
      .out(
        oneOf[PaymentResult](
          oneOfVariant[MandateCanceled.type](
            statusCode(StatusCode.Ok)
              .and(jsonBody[MandateCanceled.type].description("Mandate canceled"))
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(jsonBody[PaymentErrorMessage].description("Mandate cancelation failure"))
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[PaymentAccountNotFound.type].description("Payment account not found"))
          )
        )
      )
      .serverLogic(principal =>
        _ =>
          run(CancelMandate(externalUuidWithProfile(principal._2))).map {
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
            case other => Right((principal._1._1, principal._1._2, other))
          }
      )
      .description("Create Mandate for the authenticated payment account")

  val updateMandateStatus
    : Full[Unit, Unit, String, Unit, Either[PaymentResult, MandateResult], Any, Future] =
    rootEndpoint
      .in(PaymentSettings.MandateRoute)
      .get
      .in(query[String]("MandateId").description("Mandate Id"))
      .out(
        oneOf[Either[PaymentResult, MandateResult]](
          oneOfVariantValueMatcher[Right[PaymentResult, MandateResult]](
            statusCode(StatusCode.Ok).and(jsonBody[Right[PaymentResult, MandateResult]])
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[MandateResult]("Mandate status update failure"),
          oneOfVariantValueMatcher[Left[PaymentAccountNotFound.type, MandateResult]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, MandateResult]]
                  .description("Payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          }
        )
      )
      .description("Update mandate status web hook")
      .serverLogic(mandateId =>
        run(UpdateMandateStatus(mandateId)).map {
          case r: MandateStatusUpdated => Right(Right(r.result))
          case r: PaymentError         => Right(Left(PaymentErrorMessage(r.message)))
          case other                   => Right(Left(other))
        }
      )

  val mandateEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      createMandate,
      updateMandateStatus,
      cancelMandate
    )
}
