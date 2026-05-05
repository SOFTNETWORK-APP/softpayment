package app.softnetwork.payment.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NaturalUserDecoratorSpec extends AnyWordSpec with Matchers {

  val baseUser: NaturalUser = NaturalUser.defaultInstance
    .withFirstName("John")
    .withLastName("Doe")
    .withEmail("john@example.com")
    .withNationality("FR")
    .withCountryOfResidence("FR")
    .withBirthday("01/01/1990")
    .withExternalUuid("ext-123")
    .withPhone("+33612345678")
    .withName("ACME Corp")
    .withTitle("Mr")
    .withNaturalUserType(NaturalUser.NaturalUserType.PAYER)
    .withAddress(
      Address.defaultInstance
        .withAddressLine("17 rue Test")
        .withCity("Paris")
        .withPostalCode("75015")
        .withCountry("FR")
    )
    .withBusiness(
      Business.defaultInstance
        .withMerchantCategoryCode("5817")
        .withWebsite("https://www.example.com")
    )
    .withAdditionalProperties(Map("vatNumber" -> "FR12345678901"))

  "hasSameProviderInfo" should {

    "return true for identical users" in {
      baseUser.hasSameProviderInfo(baseUser) shouldBe true
    }

    "return true when only non-provider fields differ" in {
      val other = baseUser.copy(
        externalUuid = "different-uuid",
        birthday = "02/02/1985",
        userId = Some("cus_123"),
        walletId = Some("wal_456"),
        profile = Some("customer")
      )
      baseUser.hasSameProviderInfo(other) shouldBe true
    }

    "return false when firstName differs" in {
      val other = baseUser.copy(firstName = "Jane")
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when lastName differs" in {
      val other = baseUser.copy(lastName = "Smith")
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when email differs" in {
      val other = baseUser.copy(email = "jane@example.com")
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when name differs" in {
      val other = baseUser.copy(name = Some("New Corp"))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when name is removed" in {
      val other = baseUser.copy(name = None)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when phone differs" in {
      val other = baseUser.copy(phone = Some("+33698765432"))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when phone is removed" in {
      val other = baseUser.copy(phone = None)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when title differs" in {
      val other = baseUser.copy(title = Some("Mrs"))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when title is removed" in {
      val other = baseUser.copy(title = None)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when address differs" in {
      val other = baseUser.copy(address = Some(
        Address.defaultInstance
          .withAddressLine("42 rue Autre")
          .withCity("Lyon")
          .withPostalCode("69001")
          .withCountry("FR")
      ))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when address is removed" in {
      val other = baseUser.copy(address = None)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when address state differs" in {
      val other = baseUser.copy(address = Some(
        Address.defaultInstance
          .withAddressLine("17 rue Test")
          .withCity("Paris")
          .withPostalCode("75015")
          .withCountry("FR")
          .copy(state = Some("Ile-de-France"))
      ))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when nationality differs" in {
      val other = baseUser.copy(nationality = "DE")
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when countryOfResidence differs" in {
      val other = baseUser.copy(countryOfResidence = "BE")
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when naturalUserType differs" in {
      val other =
        baseUser.copy(naturalUserType = Some(NaturalUser.NaturalUserType.COLLECTOR))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when naturalUserType is removed" in {
      val other = baseUser.copy(naturalUserType = None)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when business differs" in {
      val other = baseUser.copy(business = Some(
        Business.defaultInstance
          .withMerchantCategoryCode("7372")
          .withWebsite("https://www.other.com")
      ))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when business is removed" in {
      val other = baseUser.copy(business = None)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when additionalProperties differ" in {
      val other =
        baseUser.copy(additionalProperties = Map("vatNumber" -> "FR99999999999"))
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when additionalProperties has extra key" in {
      val other = baseUser.copy(additionalProperties =
        baseUser.additionalProperties + ("newKey" -> "value")
      )
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return false when additionalProperties is empty" in {
      val other = baseUser.copy(additionalProperties = Map.empty)
      baseUser.hasSameProviderInfo(other) shouldBe false
    }

    "return true when both have None for optional fields" in {
      val minimal = NaturalUser.defaultInstance
        .withFirstName("John")
        .withLastName("Doe")
        .withEmail("john@example.com")
        .withNationality("FR")
        .withCountryOfResidence("FR")
      minimal.hasSameProviderInfo(minimal) shouldBe true
    }

    "return true when both have same Some values for optional fields" in {
      val a = NaturalUser.defaultInstance
        .withEmail("test@test.com")
        .withPhone("+33100000000")
        .withAddress(Address.defaultInstance.withCity("Paris").withCountry("FR"))
      val b = NaturalUser.defaultInstance
        .withEmail("test@test.com")
        .withPhone("+33100000000")
        .withAddress(Address.defaultInstance.withCity("Paris").withCountry("FR"))
      a.hasSameProviderInfo(b) shouldBe true
    }
  }
}
