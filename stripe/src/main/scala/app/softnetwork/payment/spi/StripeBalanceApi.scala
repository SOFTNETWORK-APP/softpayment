package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import com.google.gson.Gson
import com.stripe.model.Balance
import collection.JavaConverters._

trait StripeBalanceApi extends BalanceApi { self: StripeContext =>

  /** @param currency
    *   - currency
    * @param walletId
    *   - optional wallet id
    * @return
    *   balance
    */
  override def loadBalance(currency: String, walletId: Option[String]): Option[Int] = {
    // load balance
    val balances: Seq[Balance.Available] =
      Balance
        .retrieve(StripeApi().requestOptions(walletId))
        .getAvailable
        .asScala

    val availableAmount =
      balances.find(
        _.getCurrency.toLowerCase() == currency.toLowerCase
      ) match {
        case Some(balance) =>
          balance.getAmount.intValue()
        case None =>
          mlog.info(
            s"balances for ${walletId.getOrElse(self.config.clientId)} -> ${new Gson().toJson(balances)}"
          )
          0
      }

    mlog.info(
      s"balance available amount for ${walletId.getOrElse(self.config.clientId)} is $availableAmount"
    )

    Option(availableAmount)
  }

}
