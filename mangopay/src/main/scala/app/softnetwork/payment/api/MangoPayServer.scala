package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.MangoPayPaymentTypeKey

trait MangoPayServer extends PaymentServer with MangoPayPaymentTypeKey

object MangoPayServer {
  def apply(sys: ActorSystem[_]): MangoPayServer = {
    new MangoPayServer {
      override implicit val system: ActorSystem[_] = sys
    }
  }
}

