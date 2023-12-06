package app.softnetwork.payment.model

case class LegalUserDetails(
  legalUserType: LegalUser.LegalUserType,
  legalName: String,
  siret: String,
  legalRepresentativeAddress: Option[Address],
  headQuartersAddress: Option[Address]
)
