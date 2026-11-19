package app.softnetwork.payment.persistence.typed

import app.softnetwork.account.model.AccessToken
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SoftPayClientRegistrationSpec extends AnyWordSpec with Matchers {

  private def provider(apiKey: String): Provider =
    Provider.defaultInstance
      .withProviderId("ca_example")
      .withProviderApiKey(apiKey)
      .withProviderType(Provider.ProviderType.STRIPE)

  private val oldProvider = provider("sk_test_old")
  private val newProvider = provider("sk_test_new")

  private val accessToken = AccessToken.defaultInstance.withToken("access-token")

  private val storedClient: SoftPayAccount.Client =
    SoftPayAccount.Client.defaultInstance
      .withProvider(oldProvider)
      .withClientId(oldProvider.clientId)
      .withClientApiKey("hash-of-old-key")
      .withName("client name")
      .withTechnicalEmails(Seq("tech@example.com"))
      .withAccessToken(accessToken)

  private def freshClient(p: Provider): SoftPayAccount.Client =
    SoftPayAccount.Client.defaultInstance
      .withProvider(p)
      .withClientId(p.clientId)
      .withClientApiKey(s"hash-of-${p.providerApiKey}")

  "Registering a provider on an account" should {

    "replace the stored provider credentials of an existing client" in {
      val account = SoftPayAccount.defaultInstance.withClients(Seq(storedClient))
      val client =
        SoftPayClientRegistration.registeredClient(account, newProvider, freshClient(newProvider))
      client.provider.providerApiKey shouldBe "sk_test_new"
      client.getClientApiKey shouldBe "hash-of-sk_test_new"
    }

    "keep the existing client's identity and settings" in {
      val account = SoftPayAccount.defaultInstance.withClients(Seq(storedClient))
      val client =
        SoftPayClientRegistration.registeredClient(account, newProvider, freshClient(newProvider))
      client.clientId shouldBe storedClient.clientId
      client.getName shouldBe "client name"
      client.technicalEmails shouldBe Seq("tech@example.com")
      client.accessToken shouldBe Some(accessToken)
    }

    "use the new client as is when the account has no client for that provider" in {
      val other = provider("sk_test_other").withProviderId("ca_other")
      val account = SoftPayAccount.defaultInstance.withClients(Seq(freshClient(other)))
      val created = freshClient(newProvider)
      SoftPayClientRegistration.registeredClient(account, newProvider, created) shouldBe created
    }
  }
}
