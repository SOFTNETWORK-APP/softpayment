package app.softnetwork.payment.model

trait NaturalUserCompanion {
  def apply(
    firstName: String,
    lastName: String,
    email: String,
    nationality: Option[String],
    birthday: String,
    countryOfResidence: Option[String]
  ): NaturalUser = {
    NaturalUser.defaultInstance
      .withFirstName(firstName)
      .withLastName(lastName)
      .withEmail(email)
      .withNationality(nationality.getOrElse("FR"))
      .withBirthday(birthday)
      .withCountryOfResidence(countryOfResidence.getOrElse("FR"))
  }
}
