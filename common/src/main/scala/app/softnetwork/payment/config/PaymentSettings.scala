package app.softnetwork.payment.config

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging

/**
  * Created by smanciot on 05/07/2018.
  */
trait PaymentSettings extends StrictLogging {

  lazy val config: Config = ConfigFactory.load()

  val BaseUrl: String = config.getString("payment.baseUrl")

  val PaymentPath: String = config.getString("payment.path")

  val PayInRoute: String = config.getString("payment.payIn-route")
  val PayInStatementDescriptor: String = config.getString("payment.payIn-statement-descriptor")
  val PreAuthorizeCardRoute: String = config.getString("payment.pre-authorize-card-route")
  val RecurringPaymentRoute: String = config.getString("payment.recurringPayment-route")
  val SecureModeRoute: String = config.getString("payment.secure-mode-route")
  val HooksRoute: String = config.getString("payment.hooks-route")
  val MandateRoute: String = config.getString("payment.mandate-route")
  val CardRoute: String = config.getString("payment.card-route")
  val BankRoute: String = config.getString("payment.bank-route")
  val DeclarationRoute: String = config.getString("payment.declaration-route")
  val KycRoute: String = config.getString("payment.kyc-route")

  val ExternalToPaymentAccountTag: String =
    config.getString("payment.event-streams.external-to-payment-account-tag")

  val AkkaNodeRole: String = config.getString("payment.akka-node-role")
}

object PaymentSettings extends PaymentSettings
