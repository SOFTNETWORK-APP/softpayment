package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{DirectDebitTransaction, MandateResult, Transaction}

import java.util.Date

trait DirectDebitApi { _: PaymentContext =>

  /** @param externalUuid
    *   - external unique id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - Bank account id
    * @param idempotencyKey
    *   - whether to use an idempotency key for this request or not
    * @return
    *   mandate result
    */
  def mandate(
    externalUuid: String,
    userId: String,
    bankAccountId: String,
    idempotencyKey: Option[String] = None
  ): Option[MandateResult]

  /** @param maybeMandateId
    *   - optional mandate id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - bank account id
    * @return
    *   mandate associated to this bank account
    */
  def loadMandate(
    maybeMandateId: Option[String],
    userId: String,
    bankAccountId: String
  ): Option[MandateResult]

  /** @param mandateId
    *   - Provider mandate id
    * @return
    *   mandate result
    */
  def cancelMandate(mandateId: String): Option[MandateResult]

  /** @param maybeDirectDebitTransaction
    *   - direct debit transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   direct debit transaction result
    */
  def directDebit(
    maybeDirectDebitTransaction: Option[DirectDebitTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param walletId
    *   - Provider wallet id
    * @param transactionId
    *   - Provider transaction id
    * @param transactionDate
    *   - Provider transaction date
    * @return
    *   transaction if it exists
    */
  def loadDirectDebitTransaction(
    walletId: String,
    transactionId: String,
    transactionDate: Date
  ): Option[Transaction]
}
