package app.softnetwork.payment.handlers

import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.payment.message.PaymentMessages.PaymentCommand
import app.softnetwork.payment.persistence.typed.MockPaymentBehavior
import app.softnetwork.persistence.typed.CommandTypeKey

import scala.reflect.ClassTag

trait MockPaymentTypeKey extends CommandTypeKey[PaymentCommand] {
  override def TypeKey(implicit tTag: ClassTag[PaymentCommand]): EntityTypeKey[PaymentCommand] =
    MockPaymentBehavior.TypeKey
}
