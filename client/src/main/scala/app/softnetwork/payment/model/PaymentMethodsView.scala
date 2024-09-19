package app.softnetwork.payment.model

case class PaymentMethodsView(cards: Seq[CardView], paypals: Seq[Paypal])

object PaymentMethodsView {
  def apply(paymentMethods: Seq[PaymentMethod]): PaymentMethodsView = {
    val cards = paymentMethods.collect { case card: Card => card.view }
    val paypals = paymentMethods.collect { case paypal: Paypal => paypal }
    PaymentMethodsView(cards, paypals)
  }
}
