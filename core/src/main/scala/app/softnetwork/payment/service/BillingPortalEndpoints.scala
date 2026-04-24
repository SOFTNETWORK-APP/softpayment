package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.StatusCode
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait BillingPortalEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  val createBillingPortalSession: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.post
      .in(PaymentSettings.PaymentConfig.billingPortalRoute)
      .in(jsonBody[BillingPortalRequest])
      .out(
        statusCode(StatusCode.Ok)
          .and(jsonBody[BillingPortalSessionCreated])
      )
      .serverLogic { case (client, session) =>
        req => {
          run(
            CreateBillingPortalSession(
              externalUuidWithProfile(session),
              req.returnUrl,
              clientId = client.map(_.clientId).orElse(session.clientId)
            )
          ).map {
            case r: BillingPortalSessionCreated => Right(r)
            case other                          => Left(error(other))
          }
        }
      }
      .description("Create a billing portal session for the authenticated user")

  val billingPortalEndpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(createBillingPortalSession)
}
