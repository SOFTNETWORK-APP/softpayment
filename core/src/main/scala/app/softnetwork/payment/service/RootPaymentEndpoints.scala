package app.softnetwork.payment.service

import app.softnetwork.api.server.{ApiEndpoint, ApiErrors}
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.service.SessionEndpoints
import com.softwaremill.session.{
  GetSessionTransport,
  SetSessionTransport,
  TapirCsrfCheckMode,
  TapirSessionContinuity
}
import org.json4s.Formats
import org.softnetwork.session.model.Session
import sttp.model.headers.CookieValueWithMeta
import sttp.model.Method
import sttp.monad.FutureMonad
import sttp.tapir.server.PartialServerEndpoint
import sttp.tapir.{endpoint, extractFromRequest, Endpoint, EndpointInput}

import scala.concurrent.Future

trait RootPaymentEndpoints extends BasicPaymentService with ApiEndpoint {
  _: GenericPaymentHandler =>

  override implicit def formats: Formats = paymentFormats

  def sessionEndpoints: SessionEndpoints

  def sc: TapirSessionContinuity[Session] = sessionEndpoints.sc

  def st: SetSessionTransport = sessionEndpoints.st

  def gt: GetSessionTransport = sessionEndpoints.gt

  def checkMode: TapirCsrfCheckMode[Session] = sessionEndpoints.checkMode

  def extractRemoteAddress: EndpointInput.ExtractFromRequest[Option[String]] =
    extractFromRequest[Option[String]](req => req.connectionInfo.remote.map(_.getHostName))

  lazy val rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any] =
    endpoint
      .in(PaymentSettings.PaymentPath)

  lazy val secureEndpoint: PartialServerEndpoint[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    ApiErrors.ErrorInfo,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Any,
    Future
  ] = {
    val partial = sessionEndpoints.antiCsrfWithRequiredSession(sc, st, checkMode)
    partial.endpoint
      .in(PaymentSettings.PaymentPath)
      .out(partial.securityOutput)
      .errorOut(errors)
      .serverSecurityLogic { inputs =>
        partial.securityLogic(new FutureMonad())(inputs).map {
          case Left(_)  => Left(ApiErrors.Unauthorized("Unauthorized"))
          case Right(r) => Right((r._1, r._2))
        }
      }
  }
}
