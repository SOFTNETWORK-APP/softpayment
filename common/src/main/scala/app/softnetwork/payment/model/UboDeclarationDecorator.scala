package app.softnetwork.payment.model

/** Created by smanciot on 17/08/2018.
  */
trait UboDeclarationDecorator { self: UboDeclaration =>
  lazy val view: UboDeclarationView = UboDeclarationView(self)
}

case class UboDeclarationView(
  uboDeclarationId: String,
  status: UboDeclaration.UboDeclarationStatus,
  reason: Option[String] = None,
  message: Option[String] = None,
  createdDate: Option[java.util.Date] = None,
  ubos: Seq[UboDeclaration.UltimateBeneficialOwner] = Seq.empty
)

object UboDeclarationView {
  def apply(uboDeclaration: UboDeclaration): UboDeclarationView = {
    import uboDeclaration._
    UboDeclarationView(
      id,
      status,
      reason,
      message,
      createdDate,
      ubos
    )
  }
}
