package app.softnetwork.payment.model

trait PaymentMethod {

  def id: String

  def paymentType: Transaction.PaymentType

  def enabled: Boolean

  def view: PaymentMethodView
}

trait PaymentMethodView
