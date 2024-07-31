package app.softnetwork.payment.service

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.handlers.PaymentHandler
import sttp.tapir.Endpoint
import sttp.tapir.server.ServerEndpoint.Full

import scala.concurrent.Future

trait StripeHooksEndpoints extends HooksEndpoints with PaymentHandler with StripeEventHandler {

  override def hooks(
    rootEndpoint: Endpoint[Unit, Unit, Unit, Unit, Any]
  ): Full[Unit, Unit, _, Unit, Unit, Any, Future] =
    rootEndpoint
      .description("Stripe Payment Hooks")
      .in(query[String]("hash").description("Hash"))
      .in(header[Option[String]]("Stripe-Signature").description("Stripe signature"))
      .in(stringBody.description("Payload"))
      .post
      .serverLogic { case (hash, sig, payload) =>
        StripeApi.webHookSecret(hash) match {
          case Some(secret) =>
            sig match {
              case Some(signature) =>
                if (log.isDebugEnabled) {
                  log.debug(s"[Payment Hooks] Stripe Webhook received: $payload")
                }
                toStripeEvent(payload, signature, secret) match {
                  case Some(event) =>
                    handleStripeEvent(event)
                    Future.successful(Right(()))
                  case None =>
                    Future.successful(Left(()))
                }
            }
          case None =>
            Future.successful(Left(()))
        }
      }

}
