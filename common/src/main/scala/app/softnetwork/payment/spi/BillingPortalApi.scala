package app.softnetwork.payment.spi

trait BillingPortalApi { _: PaymentContext =>

  /** Create a billing portal session for the given user.
    *
    * @param userId
    *   \- the provider user ID
    * @param returnUrl
    *   \- the URL to redirect to after the user leaves the portal
    * @return
    *   the billing portal session URL, or None on failure
    */
  def createBillingPortalSession(userId: String, returnUrl: String): Option[String]
}
