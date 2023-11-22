package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.service.{ServiceWithSessionEndpoints, SessionMaterials}
import com.softwaremill.session.SessionConfig
import org.json4s.Formats
import org.softnetwork.session.model.Session
import sttp.model.headers.CookieValueWithMeta
import sttp.model.Method
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput
import sttp.tapir.Endpoint

import scala.concurrent.Future
import scala.language.implicitConversions

trait RootPaymentEndpoints
    extends BasicPaymentService
    with ServiceWithSessionEndpoints[PaymentCommand, PaymentResult] {
  _: GenericPaymentHandler with SessionMaterials =>

  override implicit def formats: Formats = paymentFormats

  implicit def sessionConfig: SessionConfig

  override implicit def ts: ActorSystem[_] = system

  override implicit def resultToApiError(result: PaymentResult): ApiErrors.ErrorInfo = error(result)

  lazy val rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any] =
    endpoint
      .in(PaymentSettings.PaymentPath)

  lazy val secureEndpoint: PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    Session,
    Unit,
    Any,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Unit,
    Any,
    Future
  ] =
    ApiErrors
      .withApiErrorVariants(
        hmacTokenCsrfProtection(checkMode) {
          requiredSession(sc, st)
        }
      )
      .in(PaymentSettings.PaymentPath)
}
