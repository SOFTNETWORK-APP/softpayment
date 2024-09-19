package app.softnetwork.payment.config

import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider

abstract class ProviderConfig(
  val clientId: String,
  val apiKey: String,
  val baseUrl: String,
  val version: String,
  val debug: Boolean,
  val secureModePath: String,
  val hooksPath: String,
  val mandatePath: String,
  val paypalPath: String
) {

  def `type`: Provider.ProviderType

  def paymentConfig: Payment.Config

  def withPaymentConfig(paymentConfig: Payment.Config): ProviderConfig

  lazy val callbacksUrl =
    s"""${paymentConfig.baseUrl}/$secureModePath/${paymentConfig.callbacksRoute}"""

  lazy val preAuthorizeReturnUrl = s"$callbacksUrl/${paymentConfig.preAuthorizeRoute}"

  lazy val payInReturnUrl = s"$callbacksUrl/${paymentConfig.payInRoute}"

  lazy val recurringPaymentReturnUrl = s"$callbacksUrl/${paymentConfig.recurringPaymentRoute}"

  lazy val hooksBaseUrl =
    s"""${paymentConfig.baseUrl}/$hooksPath/${paymentConfig.hooksRoute}/${`type`.name.toLowerCase}"""

  lazy val mandateReturnUrl =
    s"""${paymentConfig.baseUrl}/$mandatePath/${paymentConfig.mandateRoute}"""

//  lazy val payPalReturnUrl =
//    s"""${paymentConfig.baseUrl}/$paypalPath/${paymentConfig.payPalRoute}"""
//
//  lazy val cardReturnUrl =
//    s"""${paymentConfig.baseUrl}/$secureModePath/${paymentConfig.cardRoute}/${paymentConfig.payInRoute}"""

  lazy val softPayProvider: SoftPayAccount.Client.Provider =
    SoftPayAccount.Client.Provider.defaultInstance
      .withProviderType(`type`)
      .withProviderId(clientId)
      .withProviderApiKey(apiKey)
}
