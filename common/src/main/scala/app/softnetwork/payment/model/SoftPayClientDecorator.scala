package app.softnetwork.payment.model

import app.softnetwork.account.model.BearerTokenGenerator

trait SoftPayClientDecorator { _: SoftPayAccount.SoftPayClient =>

  def generateApiKey(): String = BearerTokenGenerator.generateSHAToken(clientId)

  lazy val view: SoftPayClientView = SoftPayClientView(this)
}

case class SoftPayClientView(
  clientId: String,
  clientApiKey: Option[String] = None,
  name: Option[String] = None,
  description: Option[String] = None,
  websiteUrl: Option[String] = None,
  logoUrl: Option[String] = None,
  technicalEmails: Seq[String] = Seq.empty,
  administrativeEmails: Seq[String] = Seq.empty,
  billingEmails: Seq[String] = Seq.empty,
  fraudEmails: Seq[String] = Seq.empty,
  vatNumber: Option[String] = None,
  address: Option[AddressView] = None
)

object SoftPayClientView {
  def apply(client: SoftPayAccount.SoftPayClient): SoftPayClientView = {
    import client._
    SoftPayClientView(
      clientId,
      clientApiKey,
      name,
      description,
      websiteUrl,
      logoUrl,
      technicalEmails,
      administrativeEmails,
      billingEmails,
      fraudEmails,
      vatNumber,
      address.map(AddressView(_))
    )
  }
}
