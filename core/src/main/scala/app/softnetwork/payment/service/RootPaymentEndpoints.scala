package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.service.{ServiceWithSessionEndpoints, SessionMaterials}
import org.json4s.Formats
import org.softnetwork.session.model.JwtClaims
import sttp.model.headers.CookieValueWithMeta
import sttp.model.Method
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput
import sttp.tapir.Endpoint

import scala.concurrent.Future
import scala.language.implicitConversions

trait RootPaymentEndpoints
    extends BasicPaymentService
    with ServiceWithSessionEndpoints[PaymentCommand, PaymentResult, JwtClaims]
    with ClientSessionEndpoints {
  _: GenericPaymentHandler with SessionMaterials[JwtClaims] =>

  override implicit def formats: Formats = paymentFormats

  override implicit def ts: ActorSystem[_] = system

  override implicit def resultToApiError(result: PaymentResult): ApiErrors.ErrorInfo = error(result)

  lazy val rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any] =
    endpoint
      .in(PaymentSettings.PaymentPath)

  lazy val requiredSessionEndpoint: PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    (Option[SoftPaymentAccount.Client], JwtClaims),
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
          requiredClientSession
        }
      )
      .in(PaymentSettings.PaymentPath)

  lazy val optionalSessionEndpoint: PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    (Option[SoftPaymentAccount.Client], Option[JwtClaims]),
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
          optionalClientSession
        }
      )
      .in(PaymentSettings.PaymentPath)

}
