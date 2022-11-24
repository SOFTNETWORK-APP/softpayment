package app.softnetwork.payment.handlers

import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.payment.message.PaymentMessages.PaymentCommand
import app.softnetwork.payment.persistence.typed.MangoPayPaymentBehavior
import app.softnetwork.persistence.typed.CommandTypeKey

import scala.reflect.ClassTag

/**
  * Created by smanciot on 22/04/2022.
  */
trait MangoPayPaymentTypeKey extends CommandTypeKey[PaymentCommand] {
  override def TypeKey(implicit tTag: ClassTag[PaymentCommand]): EntityTypeKey[PaymentCommand] =
    MangoPayPaymentBehavior.TypeKey
}
