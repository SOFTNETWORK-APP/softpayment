package app.softnetwork.payment.model

import app.softnetwork.payment.model

trait NaturalUserDecorator { self: NaturalUser =>
  lazy val externalUuidWithProfile: String = computeExternalUuidWithProfile(externalUuid, profile)

  lazy val view: NaturalUserView = NaturalUserView(self)

  /** Compare all fields relevant to payment provider synchronization. Returns true if
    * provider-relevant fields are identical.
    */
  def hasSameProviderInfo(other: NaturalUser): Boolean =
    self.firstName == other.firstName &&
    self.lastName == other.lastName &&
    self.email == other.email &&
    self.name == other.name &&
    self.phone == other.phone &&
    self.title == other.title &&
    self.address == other.address &&
    self.nationality == other.nationality &&
    self.countryOfResidence == other.countryOfResidence &&
    self.naturalUserType == other.naturalUserType &&
    self.business == other.business &&
    self.additionalProperties == other.additionalProperties
}

case class NaturalUserView(
  userId: Option[String] = None,
  firstName: String,
  lastName: String,
  email: String,
  nationality: String,
  birthday: String,
  countryOfResidence: String,
  externalUuid: String,
  profile: Option[String] = None,
  naturalUserType: Option[NaturalUser.NaturalUserType] = None,
  address: Option[Address] = None,
  phone: Option[String] = None,
  business: Option[Business] = None,
  title: Option[String] = None,
  name: Option[String] = None,
  additionalProperties: Map[String, String] = Map.empty
) extends User

object NaturalUserView {
  def apply(paymentUser: NaturalUser): NaturalUserView = {
    import paymentUser._
    NaturalUserView(
      userId,
      firstName,
      lastName,
      email,
      nationality,
      birthday,
      countryOfResidence,
      externalUuid,
      profile,
      naturalUserType,
      address,
      phone,
      business,
      title,
      paymentUser.name,
      paymentUser.additionalProperties
    )
  }
}
