package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.MockPaymentHandler

trait MockPaymentService extends MangoPayPaymentService with MockPaymentHandler

object MockPaymentService {
  def apply(_system: ActorSystem[_]): MockPaymentService = {
    new MockPaymentService {
      override implicit def system: ActorSystem[_] = _system
    }
  }
}
