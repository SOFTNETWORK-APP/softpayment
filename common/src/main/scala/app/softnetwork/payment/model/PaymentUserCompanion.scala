package app.softnetwork.payment.model

trait PaymentUserCompanion {
  def apply(firstName: String,
            lastName: String,
            email: String,
            nationality: Option[String],
            birthday: String,
            countryOfResidence: Option[String]): PaymentUser = {
    PaymentUser.defaultInstance
      .withFirstName(firstName)
      .withLastName(lastName)
      .withEmail(email)
      .withNationality(nationality.getOrElse("FR"))
      .withBirthday(birthday)
      .withCountryOfResidence(countryOfResidence.getOrElse("FR"))
  }
}
