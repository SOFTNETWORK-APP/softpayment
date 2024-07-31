package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{RecurringPayment, RecurringPaymentTransaction, Transaction}

trait RecurringPaymentApi { _: PaymentContext =>

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
  def registerRecurringCardPayment(
    userId: String,
    walletId: String,
    cardId: String,
    recurringPayment: RecurringPayment
  ): Option[RecurringPayment.RecurringCardPaymentResult]

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @param cardId
    *   - Provider card id
    * @param status
    *   - optional recurring payment status
    * @return
    *   recurring card payment registration updated result
    */
  def updateRecurringCardPaymentRegistration(
    recurringPayInRegistrationId: String,
    cardId: Option[String],
    status: Option[RecurringPayment.RecurringCardPaymentStatus]
  ): Option[RecurringPayment.RecurringCardPaymentResult]

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @return
    *   recurring card payment registration result
    */
  def loadRecurringCardPayment(
    recurringPayInRegistrationId: String
  ): Option[RecurringPayment.RecurringCardPaymentResult]

  /** @param recurringPaymentTransaction
    *   - recurring payment transaction
    * @return
    *   resulted payIn transaction
    */
  def createRecurringCardPayment(
    recurringPaymentTransaction: RecurringPaymentTransaction
  ): Option[Transaction]

}
