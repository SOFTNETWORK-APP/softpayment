package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import com.google.gson.Gson
import com.stripe.model.Balance
import collection.JavaConverters._

trait StripeBalanceApi extends BalanceApi { self: StripeContext =>

  /** @param currency
    *   - currency
    * @param creditedUserId
    *   - optional credited user id
    * @return
    *   balance
    */
  override def loadBalance(currency: String, creditedUserId: Option[String]): Option[Int] = {
    var requestOptions = StripeApi().requestOptionsBuilder

    creditedUserId match {
      case Some(value) =>
        requestOptions = requestOptions.setStripeAccount(value)
      case _ =>
    }

    // load balance
    val balances: Seq[Balance.Available] =
      Balance
        .retrieve(requestOptions.build())
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
            s"balances for ${creditedUserId.getOrElse(self.config.clientId)} -> ${new Gson().toJson(balances)}"
          )
          0
      }

    mlog.info(
      s"balance available amount for ${creditedUserId.getOrElse(self.config.clientId)} is $availableAmount"
    )

    Option(availableAmount)
  }

}
