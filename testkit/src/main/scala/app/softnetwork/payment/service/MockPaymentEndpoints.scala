package app.softnetwork.payment.service

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.SwaggerEndpoint
import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.session.service.SessionEndpoints
import org.slf4j.{Logger, LoggerFactory}

trait MockPaymentEndpoints extends MangoPayPaymentEndpoints with MockPaymentHandler

object MockPaymentEndpoints {
  def apply(
    _system: ActorSystem[_],
    _sessionEndpoints: SessionEndpoints
  ): MockPaymentEndpoints = {
    new MockPaymentEndpoints {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = _system
      override def sessionEndpoints: SessionEndpoints = _sessionEndpoints
    }
  }

  def swagger(
    _system: ActorSystem[_],
    _sessionEndpoints: SessionEndpoints
  ): SwaggerEndpoint = {
    new MockPaymentEndpoints with SwaggerEndpoint {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit def system: ActorSystem[_] = _system
      override def sessionEndpoints: SessionEndpoints = _sessionEndpoints
    }
  }
}
