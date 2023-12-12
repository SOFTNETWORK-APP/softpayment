package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.MockSoftPayAccountDao
import org.slf4j.{Logger, LoggerFactory}

trait MockClientServer extends ClientServer with MockSoftPayAccountDao

object MockClientServer {
  def apply(sys: ActorSystem[_]): MockClientServer = {
    new MockClientServer {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit val system: ActorSystem[_] = sys
    }
  }
}
