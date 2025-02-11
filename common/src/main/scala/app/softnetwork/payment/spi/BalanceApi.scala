package app.softnetwork.payment.spi

trait BalanceApi { _: PaymentContext =>

  /** @param currency
    *   - currency
    * @param creditedUserId
    *   - optional credited user id
    * @return
    *   balance
    */
  def loadBalance(currency: String, creditedUserId: Option[String]): Option[Int]

}
