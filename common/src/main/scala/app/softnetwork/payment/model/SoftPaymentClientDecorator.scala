package app.softnetwork.payment.model

import app.softnetwork.account.model.BearerTokenGenerator

trait SoftPaymentClientDecorator { _: SoftPaymentAccount.Client =>

  def generateApiKey(): String = BearerTokenGenerator.generateSHAToken(clientId)

}

case class SoftPaymentClientView(
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

object SoftPaymentClientView {
  def apply(client: SoftPaymentAccount.Client): SoftPaymentClientView = {
    import client._
    SoftPaymentClientView(
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
