package app.softnetwork.payment.spi

import app.softnetwork.payment.model._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/** Created by smanciot on 16/08/2018.
  */
private[payment] trait PaymentProvider
    extends PaymentContext
    with PaymentAccountApi
    with PreAuthorizationApi
    with PaymentMethodApi
    with DirectDebitApi
    with PayInApi
    with PayOutApi
    with TransferApi
    with RefundApi
    with RecurringPaymentApi
    with BalanceApi {

  protected lazy val mlog: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  /** @return
    *   client
    */
  def client: Option[SoftPayAccount.Client]

}
