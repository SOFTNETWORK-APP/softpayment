package app.softnetwork.payment.model

trait PaypalDecorator extends PaymentMethod with PaymentMethodView { self: Paypal =>

  override def paymentType: Transaction.PaymentType = Transaction.PaymentType.PAYPAL

  override def enabled: Boolean = active.getOrElse(true)

  override def view: PaymentMethodView = self
}
