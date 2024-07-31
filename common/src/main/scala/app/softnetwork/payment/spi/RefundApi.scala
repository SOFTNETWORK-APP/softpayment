package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{RefundTransaction, Transaction}

trait RefundApi { _: PaymentContext =>

  /** @param maybeRefundTransaction
    *   - refund transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   refund transaction result
    */
  def refund(
    maybeRefundTransaction: Option[RefundTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   Refund transaction
    */
  def loadRefundTransaction(orderUuid: String, transactionId: String): Option[Transaction]

}
