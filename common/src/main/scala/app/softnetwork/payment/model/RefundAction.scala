package app.softnetwork.payment.model

trait RefundAction

case class RefundRequired(refund: RefundTransaction) extends RefundAction

case object RefundNotRequired extends RefundAction

case object UnknownRefund extends RefundAction

