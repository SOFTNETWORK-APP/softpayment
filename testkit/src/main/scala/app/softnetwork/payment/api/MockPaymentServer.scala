package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.MockPaymentTypeKey

trait MockPaymentServer extends PaymentServer with MockPaymentTypeKey

object MockPaymentServer {
  def apply(sys: ActorSystem[_]): MockPaymentServer = {
    new MockPaymentServer {
      override implicit val system: ActorSystem[_] = sys
    }
  }
}