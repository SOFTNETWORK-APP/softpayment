package app.softnetwork.payment.model

import java.util.Locale

trait AddressDecorator { self: Address =>

  lazy val wrongAddress: Boolean =
    addressLine.trim.isEmpty ||
    city.trim.isEmpty ||
    country.trim.isEmpty ||
    postalCode.trim.isEmpty

  lazy val view: AddressView = AddressView(self)

  override def equals(obj: Any): Boolean = obj match {
    case address: Address =>
      address.addressLine.equals(addressLine) &&
        address.city.equals(city) &&
        address.country.equals(country) &&
        address.postalCode.equals(postalCode) &&
        address.state.getOrElse("").equals(state.getOrElse(""))
    case _ => super.equals(obj)
  }

  def countryName(language: String = "fr"): String =
    new Locale(language, country.toUpperCase).getDisplayCountry
}

case class AddressView(
  addressLine: String,
  city: String,
  postalCode: String,
  country: String,
  state: Option[String]
)

object AddressView {
  def apply(address: Address): AddressView = {
    import address._
    AddressView(addressLine, city, postalCode, country, state)
  }
}
