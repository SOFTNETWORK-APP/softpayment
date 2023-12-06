package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.{
  MockPaymentHandler,
  MockPaymentTypeKey,
  MockSoftPaymentAccountDao,
  SoftPaymentAccountDao
}
import org.slf4j.{Logger, LoggerFactory}

trait MockClientServer extends ClientServer with MockPaymentHandler

object MockClientServer {
  def apply(sys: ActorSystem[_]): MockClientServer = {
    new MockClientServer {
      override def softPaymentAccountDao: SoftPaymentAccountDao = MockSoftPaymentAccountDao
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit val system: ActorSystem[_] = sys
    }
  }
}
