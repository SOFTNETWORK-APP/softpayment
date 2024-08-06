package app.softnetwork.payment

import app.softnetwork.payment.model.{
  Address,
  Business,
  BusinessSupport,
  CardPreRegistration,
  LegalUser,
  NaturalUser
}
import app.softnetwork.payment.model.UboDeclaration.UltimateBeneficialOwner
import app.softnetwork.payment.model.UboDeclaration.UltimateBeneficialOwner.BirthPlace

package object data {

  val orderUuid = "order"

  val customerUuid = "customer"

  val sellerUuid = "seller"

  val vendorUuid = "vendor"

  var cardPreRegistration: CardPreRegistration = _

  var preAuthorizationId: String = _

  var recurringPaymentRegistrationId: String = _

  /** natural user */
  val firstName = "firstName"
  val lastName = "lastName"
  val birthday = "26/12/1972"
  val email = "demo@softnetwork.fr"
  val phone = "+33102030405"

  val business: Business = Business.defaultInstance
    .withMerchantCategoryCode("5817")
    .withWebsite("https://www.softnetwork.fr")
    .withSupport(BusinessSupport.defaultInstance.withEmail(email).withPhone(phone))

  val ownerName = s"$firstName $lastName"
  val ownerAddress: Address = Address.defaultInstance
    .withAddressLine("17 rue Bouilloux Lafont")
    .withCity("Paris")
    .withPostalCode("75015")
    .withCountry("FR")

  val naturalUser: NaturalUser =
    NaturalUser.defaultInstance
      .withExternalUuid(customerUuid)
      .withFirstName(firstName)
      .withLastName(lastName)
      .withBirthday(birthday)
      .withAddress(ownerAddress)
      .withEmail(email)
      .withPhone(phone)
      .withBusiness(business)

  /** bank account */
  var sellerBankAccountId: String = _
  var vendorBankAccountId: String = _
  val iban = "FR1420041010050500013M02606"
  val bic = "SOGEFRPPPSZ"

  /** legal user */
  val siret = "732 829 320 00074"
  val vatNumber = "FR49732829320"
  val legalUser: LegalUser = LegalUser.defaultInstance
    .withSiret(siret)
    .withVatNumber(vatNumber)
    .withLegalName(ownerName)
    .withLegalUserType(LegalUser.LegalUserType.SOLETRADER)
    .withLegalRepresentative(naturalUser.withExternalUuid(sellerUuid))
    .withLegalRepresentativeAddress(ownerAddress)
    .withHeadQuartersAddress(ownerAddress)
    .withPhone(phone)
    .withBusiness(business)

  /** ultimate beneficial owner */
  val ubo: UltimateBeneficialOwner = UltimateBeneficialOwner.defaultInstance
    .withFirstName(s"owner$firstName")
    .withLastName(s"owner$lastName")
    .withBirthday(birthday)
    .withBirthPlace(BirthPlace.defaultInstance.withCity("city"))
    .withAddress(ownerAddress.addressLine)
    .withCity(ownerAddress.city)
    .withPostalCode(ownerAddress.postalCode)
    .withPercentOwnership(100.0)
    .withPhone(phone)
    .withEmail(email)

  var uboDeclarationId: String = _

  var cardId: String = _

  var mandateId: String = _

  var directDebitTransactionId: String = _

  val debitedAmount: Int = 5100

  val feesAmount: Int = debitedAmount * 10 / 100

}
