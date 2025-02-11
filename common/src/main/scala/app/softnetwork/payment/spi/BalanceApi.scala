package app.softnetwork.payment.spi

trait BalanceApi { _: PaymentContext =>

  /** @param currency
    *   - currency
    * @param walletId
    *   - optional wallet id
    * @return
    *   balance
    */
  def loadBalance(currency: String, walletId: Option[String]): Option[Int]

  /** @param currency
    *   - optional currency
    * @return
    *   client fees
    */
  def clientFees(currency: Option[String] = None): Option[Double] =
    loadBalance(currency.getOrElse("EUR"), None) match {
      case Some(balance) =>
        Some(balance.toDouble / 100)
      case None =>
        None
    }

}
