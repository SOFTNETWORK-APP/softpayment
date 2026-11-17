package app.softnetwork.payment.config

import app.softnetwork.payment.model.SoftPayAccount.Client
import app.softnetwork.security.sha256
import com.stripe.param.{WebhookEndpointCreateParams, WebhookEndpointUpdateParams}
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

    "subscribe the webhook endpoint to the same events on create and update" in {
      // The events the endpoint subscribed to before the shared list existed, in the same order.
      StripeApi.webhookCreateEvents shouldBe Seq(
        WebhookEndpointCreateParams.EnabledEvent.ACCOUNT__UPDATED,
        WebhookEndpointCreateParams.EnabledEvent.PERSON__UPDATED,
        WebhookEndpointCreateParams.EnabledEvent.INVOICE__PAYMENT_SUCCEEDED,
        WebhookEndpointCreateParams.EnabledEvent.INVOICE__PAYMENT_FAILED,
        WebhookEndpointCreateParams.EnabledEvent.CUSTOMER__SUBSCRIPTION__DELETED,
        WebhookEndpointCreateParams.EnabledEvent.CUSTOMER__SUBSCRIPTION__UPDATED,
        WebhookEndpointCreateParams.EnabledEvent.CUSTOMER__UPDATED,
        WebhookEndpointCreateParams.EnabledEvent.PAYMENT_METHOD__ATTACHED,
        WebhookEndpointCreateParams.EnabledEvent.PAYMENT_METHOD__DETACHED
      )
      StripeApi.webhookUpdateEvents shouldBe Seq(
        WebhookEndpointUpdateParams.EnabledEvent.ACCOUNT__UPDATED,
        WebhookEndpointUpdateParams.EnabledEvent.PERSON__UPDATED,
        WebhookEndpointUpdateParams.EnabledEvent.INVOICE__PAYMENT_SUCCEEDED,
        WebhookEndpointUpdateParams.EnabledEvent.INVOICE__PAYMENT_FAILED,
        WebhookEndpointUpdateParams.EnabledEvent.CUSTOMER__SUBSCRIPTION__DELETED,
        WebhookEndpointUpdateParams.EnabledEvent.CUSTOMER__SUBSCRIPTION__UPDATED,
        WebhookEndpointUpdateParams.EnabledEvent.CUSTOMER__UPDATED,
        WebhookEndpointUpdateParams.EnabledEvent.PAYMENT_METHOD__ATTACHED,
        WebhookEndpointUpdateParams.EnabledEvent.PAYMENT_METHOD__DETACHED
      )
      StripeApi.webhookEnabledEvents.distinct shouldBe StripeApi.webhookEnabledEvents
    }
  }
}
