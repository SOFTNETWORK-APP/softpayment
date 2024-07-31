package app.softnetwork.payment.spi

import app.softnetwork.payment.model.{Transaction, TransferTransaction}

trait TransferApi { _: PaymentContext =>

  /** @param maybeTransferTransaction
    *   - transfer transaction
    * @return
    *   transfer transaction result
    */
  def transfer(maybeTransferTransaction: Option[TransferTransaction]): Option[Transaction]

  /** @param transactionId
    *   - transaction id
    * @return
    *   transfer transaction
    */
  def loadTransfer(transactionId: String): Option[Transaction]

}
