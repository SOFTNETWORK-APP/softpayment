package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiErrors
import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.session.model.{SessionData, SessionDataCompanion, SessionDataDecorator}
import app.softnetwork.session.service.{ServiceWithSessionEndpoints, SessionMaterials}
import com.softwaremill.session.SessionConfig
import org.json4s.Formats
import sttp.model.headers.CookieValueWithMeta
import sttp.model.Method
import sttp.tapir.server.PartialServerEndpointWithSecurityOutput
import sttp.tapir.Endpoint

import scala.concurrent.Future
import scala.language.implicitConversions

trait RootPaymentEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends BasicPaymentService[SD]
    with ServiceWithSessionEndpoints[PaymentCommand, PaymentResult, SD] {
  _: GenericPaymentHandler with SessionMaterials[SD] =>

  override implicit def formats: Formats = paymentFormats

  override implicit def ts: ActorSystem[_] = system

  implicit def sessionConfig: SessionConfig

  implicit def companion: SessionDataCompanion[SD]

  override implicit def resultToApiError(result: PaymentResult): ApiErrors.ErrorInfo = error(result)

  lazy val rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any] =
    endpoint
      .in(PaymentSettings.PaymentPath)

  lazy val secureEndpoint: PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    SD,
    Unit,
    Any,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Unit,
    Any,
    Future
  ] =
    ApiErrors
      .withApiErrorVariants(
        antiCsrfWithRequiredSession(sc, gt, checkMode)
      )
      .in(PaymentSettings.PaymentPath)
}
