package app.softnetwork.payment.service

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.handlers.PaymentHandler

trait StripeHooksDirectives extends HooksDirectives with PaymentHandler with StripeEventHandler {

  override def hooks: Route = pathEnd {
    parameter("hash") { hash =>
      StripeApi.webHookSecret(hash) match {
        case Some(secret) =>
          optionalHeaderValueByName("Stripe-Signature") {
            case Some(signature) =>
              entity(as[String]) { payload =>
                if (log.isDebugEnabled) {
                  log.debug(s"[Payment Hooks] Stripe Webhook received: $payload")
                }
                toStripeEvent(payload, signature, secret) match {
                  case Some(event) =>
                    handleStripeEvent(event)
                    complete(HttpResponse(StatusCodes.OK))
                  case None =>
                    complete(HttpResponse(StatusCodes.NotFound))
                }
              }
            case None =>
              complete(HttpResponse(StatusCodes.NotFound))
          }
        case None =>
          complete(HttpResponse(StatusCodes.NotFound))
      }
    }
  }

}
