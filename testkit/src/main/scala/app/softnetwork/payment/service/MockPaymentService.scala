package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.session.service.SessionService
import org.slf4j.{Logger, LoggerFactory}

trait MockPaymentService extends MangoPayPaymentService with MockPaymentHandler

object MockPaymentService {
  def apply(_system: ActorSystem[_], _sessionService: SessionService): MockPaymentService = {
    new MockPaymentService {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = _system
      override def sessionService: SessionService = _sessionService
    }
  }
}
