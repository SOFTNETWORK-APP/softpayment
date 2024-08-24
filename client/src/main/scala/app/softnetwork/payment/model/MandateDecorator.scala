package app.softnetwork.payment.model

trait MandateDecorator { self: Mandate =>
  lazy val view: MandateView = MandateView(this)
}

case class MandateView(
  id: String,
  status: Mandate.MandateStatus,
  scheme: Mandate.MandateScheme,
  resultCode: Option[String] = None,
  resultMessage: Option[String] = None
)

object MandateView {
  def apply(mandate: Mandate): MandateView = {
    import mandate._
    MandateView(id, mandateStatus, mandateScheme, resultCode, resultMessage)
  }
}
