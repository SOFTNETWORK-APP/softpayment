package app.softnetwork.payment.scalatest

import app.softnetwork.payment.config.{StripeApi, StripeSettings}
import org.scalatest.Suite

trait StripePaymentTestKit extends PaymentTestKit { _: Suite =>

  override implicit lazy val providerConfig: StripeApi.Config = StripeSettings.StripeApiConfig

}
