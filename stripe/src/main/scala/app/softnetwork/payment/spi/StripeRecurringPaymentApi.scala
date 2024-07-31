package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{RecurringPayment, RecurringPaymentTransaction, Transaction}
import com.stripe.param.SubscriptionScheduleCreateParams

trait StripeRecurringPaymentApi extends RecurringPaymentApi { _: StripeContext =>

  /** @param userId
    *   - Provider user id
    * @param walletId
    *   - Provider wallet id
    * @param cardId
    *   - Provider card id
    * @param recurringPayment
    *   - recurring payment to register
    * @return
    *   recurring card payment registration result
    */
  override def registerRecurringCardPayment(
    userId: String,
    walletId: String,
    cardId: String,
    recurringPayment: RecurringPayment
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    SubscriptionScheduleCreateParams
      .builder()
      .addPhase(
        SubscriptionScheduleCreateParams.Phase
          .builder()
          .setBillingCycleAnchor(
            SubscriptionScheduleCreateParams.Phase.BillingCycleAnchor.AUTOMATIC
          )
          .build()
      )
    ???
  }

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @param cardId
    *   - Provider card id
    * @param status
    *   - optional recurring payment status
    * @return
    *   recurring card payment registration updated result
    */
  override def updateRecurringCardPaymentRegistration(
    recurringPayInRegistrationId: String,
    cardId: Option[String],
    status: Option[RecurringPayment.RecurringCardPaymentStatus]
  ): Option[RecurringPayment.RecurringCardPaymentResult] = ???

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @return
    *   recurring card payment registration result
    */
  override def loadRecurringCardPayment(
    recurringPayInRegistrationId: String
  ): Option[RecurringPayment.RecurringCardPaymentResult] = ???

  /** @param recurringPaymentTransaction
    *   - recurring payment transaction
    * @return
    *   resulted payIn transaction
    */
  override def createRecurringCardPayment(
    recurringPaymentTransaction: RecurringPaymentTransaction
  ): Option[Transaction] = ???
}
