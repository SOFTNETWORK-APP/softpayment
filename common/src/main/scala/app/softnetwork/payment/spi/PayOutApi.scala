package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{PayOutTransaction, Transaction}

trait PayOutApi { _: PaymentContext =>

  /** @param maybePayOutTransaction
    *   - pay out transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay out transaction result
    */
  def payOut(
    maybePayOutTransaction: Option[PayOutTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay out transaction
    */
  def loadPayOutTransaction(orderUuid: String, transactionId: String): Option[Transaction]

}
