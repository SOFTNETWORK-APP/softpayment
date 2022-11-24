package app.softnetwork.payment.model

import app.softnetwork.validation.RegexValidator

import scala.util.matching.Regex

trait LegalUserDecorator {self: LegalUser =>
  lazy val  wrongSiret: Boolean = !SiretValidator.check(siret)

  lazy val wrongLegalRepresentativeAddress: Boolean = legalRepresentativeAddress.wrongAddress

  lazy val wrongHeadQuartersAddress: Boolean = headQuartersAddress.wrongAddress

  lazy val uboDeclarationRequired: Boolean = legalUserType.isBusiness

  lazy val uboDeclarationValidated: Boolean = !uboDeclarationRequired ||
    uboDeclaration.exists(_.status.isUboDeclarationValidated)

  lazy val view: LegalUserView = LegalUserView(self)
}

object SiretValidator extends RegexValidator {
  val regex: Regex = """^[0-9]{14}""".r
}

case class LegalUserView(legalUserType: LegalUser.LegalUserType,
                         legalName: String,
                         siret: String,
                         legalRepresentative: PaymentUserView,
                         legalRepresentativeAddress: AddressView,
                         headQuartersAddress: AddressView,
                         uboDeclaration: Option[UboDeclarationView] = None,
                         lastAcceptedTermsOfPSP: Option[java.util.Date] = None
                        )

object LegalUserView{
  def apply(legalUser: LegalUser): LegalUserView = {
    import legalUser._
    LegalUserView(
      legalUserType,
      legalName,
      siret,
      legalRepresentative.view,
      legalRepresentativeAddress.view,
      headQuartersAddress.view,
      uboDeclaration.map(_.view),
      lastAcceptedTermsOfPSP
    )
  }
}