package app.softnetwork.payment.model

import app.softnetwork.payment.model
import app.softnetwork.validation.RegexValidator

import scala.util.matching.Regex

trait LegalUserDecorator { self: LegalUser =>
  lazy val wrongSiret: Boolean = !SiretValidator.check(siret.split(" ").mkString)

  lazy val wrongLegalRepresentativeAddress: Boolean = legalRepresentativeAddress.wrongAddress

  lazy val wrongHeadQuartersAddress: Boolean = headQuartersAddress.wrongAddress

  lazy val uboDeclarationRequired: Boolean = legalUserType.isBusiness

  lazy val uboDeclarationValidated: Boolean = !uboDeclarationRequired ||
    uboDeclaration.exists(_.status.isUboDeclarationValidated)

  lazy val view: LegalUserView = model.LegalUserView(self)

  lazy val siren: String = siret.split(" ").mkString.take(9)
}

object SiretValidator extends RegexValidator {
  val regex: Regex = """^[0-9]{14}""".r
}

case class LegalUserView(
  legalUserType: LegalUser.LegalUserType,
  legalName: String,
  siret: String,
  siren: String,
  legalRepresentative: NaturalUserView,
  legalRepresentativeAddress: AddressView,
  headQuartersAddress: AddressView,
  uboDeclaration: Option[UboDeclarationView] = None,
  lastAcceptedTermsOfPSP: Option[java.util.Date] = None
) extends User {
  override lazy val userId: Option[String] = legalRepresentative.userId
}

object LegalUserView {
  def apply(legalUser: LegalUser): LegalUserView = {
    import legalUser._
    LegalUserView(
      legalUserType,
      legalName,
      siret,
      siren,
      legalRepresentative.view,
      legalRepresentativeAddress.view,
      headQuartersAddress.view,
      uboDeclaration.map(_.view),
      lastAcceptedTermsOfPSP
    )
  }
}
