package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{
  PayInTransaction,
  PayInWithCardPreAuthorizedTransaction,
  PayInWithCardTransaction,
  PayInWithPayPalTransaction,
  Transaction
}

trait PayInApi { _: PaymentContext =>

  /** @param payInTransaction
    *   - pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in transaction result
    */
  def payIn(
    payInTransaction: Option[PayInTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] = {
    payInTransaction match {
      case Some(payInTransaction) =>
        payInTransaction.paymentType match {
          case Transaction.PaymentType.CARD =>
            payInWithCard(payInTransaction.cardTransaction, idempotency)
          case Transaction.PaymentType.PREAUTHORIZED =>
            payInWithCardPreAuthorized(payInTransaction.cardPreAuthorizedTransaction, idempotency)
          case Transaction.PaymentType.PAYPAL =>
            payInWithPayPal(payInTransaction.payPalTransaction, idempotency)
          case _ => None
        }
      case None => None
    }
  }

  /** @param payInWithCardPreAuthorizedTransaction
    *   - card pre authorized pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with card pre authorized transaction result
    */
  private[spi] def payInWithCardPreAuthorized(
    payInWithCardPreAuthorizedTransaction: Option[PayInWithCardPreAuthorizedTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param maybePayInTransaction
    *   - pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in transaction result
    */
  private[spi] def payInWithCard(
    maybePayInTransaction: Option[PayInWithCardTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param payInWithPayPalTransaction
    *   - pay in with PayPal transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with PayPal transaction result
    */
  private[spi] def payInWithPayPal(
    payInWithPayPalTransaction: Option[PayInWithPayPalTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay in transaction
    */
  def loadPayInTransaction(
    orderUuid: String,
    transactionId: String,
    recurringPayInRegistrationId: Option[String]
  ): Option[Transaction]

}
