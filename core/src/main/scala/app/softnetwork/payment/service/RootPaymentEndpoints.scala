package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages.{
  PaymentError,
  PaymentErrorMessage,
  UnauthorizedError
}
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.service.SessionEndpoints
import org.json4s.Formats
import org.softnetwork.session.model.Session
import sttp.model.{Method, StatusCode}
import sttp.model.headers.CookieValueWithMeta
import sttp.monad.FutureMonad
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.PartialServerEndpoint
import sttp.tapir.{
  emptyOutputAs,
  endpoint,
  extractFromRequest,
  oneOf,
  oneOfVariant,
  oneOfVariantValueMatcher,
  statusCode,
  Endpoint,
  EndpointInput,
  EndpointOutput,
  Schema
}

import scala.concurrent.Future

trait RootPaymentEndpoints extends BasicPaymentService { _: GenericPaymentHandler =>

  import app.softnetwork.serialization._

  implicit def formats: Formats = paymentFormats

  def sessionEndpoints: SessionEndpoints

  def oneOfPaymentErrorMessageValueMatcher[T: Manifest: Schema](
    description: String,
    status: StatusCode = StatusCode.BadRequest
  ): EndpointOutput.OneOfVariant[Left[PaymentErrorMessage, T]] =
    oneOfVariantValueMatcher[Left[PaymentErrorMessage, T]](
      statusCode(status)
        .and(jsonBody[Left[PaymentErrorMessage, T]].description(description))
    ) { case Left(PaymentErrorMessage(_)) =>
      true
    }

  def extractRemoteAddress: EndpointInput.ExtractFromRequest[Option[String]] =
    extractFromRequest[Option[String]](req => req.connectionInfo.remote.map(_.getHostName))

  lazy val rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any] =
    endpoint
      .in(PaymentSettings.PaymentPath)

  lazy val secureEndpoint: PartialServerEndpoint[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Any,
    Future
  ] =
    sessionEndpoints.antiCsrfWithRequiredSession.endpoint
      .in(PaymentSettings.PaymentPath)
      .out(sessionEndpoints.antiCsrfWithRequiredSession.securityOutput)
      .errorOut(
        oneOf[PaymentError](
          oneOfVariant[UnauthorizedError.type](
            statusCode(StatusCode.Unauthorized)
              .and(emptyOutputAs(UnauthorizedError).description("Unauthorized"))
          )
        )
      )
      .serverSecurityLogic { inputs =>
        sessionEndpoints.antiCsrfWithRequiredSession.securityLogic(new FutureMonad())(inputs).map {
          case Left(_)  => Left(UnauthorizedError)
          case Right(r) => Right((r._1, r._2))
        }
      }
}
