package app.softnetwork.payment.model

trait PaymentUserDecorator { self: PaymentUser =>
  lazy val externalUuidWithProfile: String = computeExternalUuidWithProfile(externalUuid, profile)

  lazy val view: PaymentUserView = PaymentUserView(self)
}

case class PaymentUserView(
  userId: Option[String] = None,
  firstName: String,
  lastName: String,
  email: String,
  nationality: String,
  birthday: String,
  countryOfResidence: String,
  externalUuid: String,
  profile: Option[String] = None,
  paymentUserType: Option[PaymentUser.PaymentUserType] = None
)

object PaymentUserView {
  def apply(paymentUser: PaymentUser): PaymentUserView = {
    import paymentUser._
    PaymentUserView(
      userId,
      firstName,
      lastName,
      email,
      nationality,
      birthday,
      countryOfResidence,
      externalUuid,
      profile,
      paymentUserType
    )
  }
}
