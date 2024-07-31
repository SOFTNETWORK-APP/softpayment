package app.softnetwork.payment.config

import app.softnetwork.payment.model.SoftPayAccount.Client
import app.softnetwork.security.sha256
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StripeApiSpec extends AnyWordSpec with Matchers {

  "StripeApi" should {
    "be able to create a new instance" in {
      implicit def config: StripeApi.Config = StripeSettings.StripeApiConfig
      implicit def provider: Client.Provider = config.softPayProvider
      StripeApi() should not be null
      StripeApi.webHookSecret(sha256(provider.clientId)).isDefined shouldBe true
    }
  }
}
