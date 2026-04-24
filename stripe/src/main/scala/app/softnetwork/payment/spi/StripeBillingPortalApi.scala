package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi

import scala.util.{Failure, Success, Try}

trait StripeBillingPortalApi extends BillingPortalApi { _: StripeContext =>

  override def createBillingPortalSession(userId: String, returnUrl: String): Option[String] = {
    Try {
      val params = com.stripe.param.billingportal.SessionCreateParams
        .builder()
        .setCustomer(userId)
        .setReturnUrl(returnUrl)
        .build()
      com.stripe.model.billingportal.Session.create(params, StripeApi().requestOptions())
    } match {
      case Success(session) => Some(session.getUrl)
      case Failure(f) =>
        mlog.error(s"Failed to create billing portal session for $userId: ${f.getMessage}", f)
        None
    }
  }
}
