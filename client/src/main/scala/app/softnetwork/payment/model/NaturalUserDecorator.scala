package app.softnetwork.payment.model

import app.softnetwork.payment.model

trait NaturalUserDecorator { self: NaturalUser =>
  lazy val externalUuidWithProfile: String = computeExternalUuidWithProfile(externalUuid, profile)

  lazy val view: NaturalUserView = NaturalUserView(self)
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
  naturalUserType: Option[NaturalUser.NaturalUserType] = None
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
      naturalUserType
    )
  }
}
