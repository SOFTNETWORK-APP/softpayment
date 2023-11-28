package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.handlers.{MangoPayPaymentTypeKey, SoftPaymentAccountDao}
import org.slf4j.{Logger, LoggerFactory}

trait MangoPayServer extends PaymentServer with MangoPayPaymentTypeKey

object MangoPayServer {
  def apply(sys: ActorSystem[_]): MangoPayServer = {
    new MangoPayServer {
      lazy val log: Logger = LoggerFactory getLogger getClass.getName
      override implicit val system: ActorSystem[_] = sys
    }
  }
}
