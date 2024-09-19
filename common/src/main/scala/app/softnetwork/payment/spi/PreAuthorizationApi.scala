package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{PreAuthorizationTransaction, Transaction}

trait PreAuthorizationApi { _: PaymentContext =>

  /** @param preAuthorizationTransaction
    *   - pre authorization
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pre authorized transaction result
    */
  def preAuthorize(
    preAuthorizationTransaction: PreAuthorizationTransaction,
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param preAuthorizedTransactionId
    *   - pre authorized transaction id
    * @return
    *   pre authorized transaction
    */
  def loadPreAuthorization(
    orderUuid: String,
    preAuthorizedTransactionId: String
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param preAuthorizedTransactionId
    *   - pre authorized transaction id
    * @return
    *   whether pre authorized transaction has been cancelled or not
    */
  def cancelPreAuthorization(
    orderUuid: String,
    preAuthorizedTransactionId: String
  ): Boolean

  /** @param orderUuid
    *   - order unique id
    * @param preAuthorizedTransactionId
    *   - pre authorized transaction id
    * @return
    *   whether pre authorized transaction has been validated or not
    */
  def validatePreAuthorization(
    orderUuid: String,
    preAuthorizedTransactionId: String
  ): Boolean

}
