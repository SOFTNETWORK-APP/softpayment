package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{PaymentMethod, PreRegistration, Transaction}

trait PaymentMethodApi { _: PaymentContext =>

  /** @param maybeUserId
    *   - owner of the payment method
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @param paymentType
    *   - payment type
    * @return
    *   pre registration
    */
  def preRegisterPaymentMethod(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String,
    paymentType: Transaction.PaymentType
  ): Option[PreRegistration]

  /** @param registrationId
    *   - payment method registration id
    * @param maybeRegistrationData
    *   - optional registration data
    * @return
    *   payment method id
    */
  def registerPaymentMethod(
    registrationId: String,
    maybeRegistrationData: Option[String]
  ): Option[String]

  /** @param paymentMethodId
    *   - payment method id
    * @return
    *   payment method or none
    */
  def loadPaymentMethod(paymentMethodId: String): Option[PaymentMethod]

  /** attach a payment method to a user
    * @param paymentMethodId
    *   - payment method id to register
    * @param userId
    *   - owner of the payment method
    * @return
    *   payment method attached
    */
  def attachPaymentMethod(
    paymentMethodId: String,
    userId: String
  ): Option[PaymentMethod] = None

  /** Disable a payment method
    * @param paymentMethodId
    *   - id of payment method to disable
    * @return
    *   payment method disabled or none
    */
  def disablePaymentMethod(paymentMethodId: String): Option[PaymentMethod]
}
