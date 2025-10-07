package app.softnetwork.payment.config

object Payment {

  case class Config(
    baseUrl: String,
    path: String,
    payInRoute: String,
    payInStatementDescriptor: String,
    preAuthorizeRoute: String,
    recurringPaymentRoute: String,
    callbacksRoute: String,
    hooksRoute: String,
    mandateRoute: String,
    cardRoute: String,
    paymentMethodRoute: String,
    bankRoute: String,
    accountRoute: String,
    declarationRoute: String,
    kycRoute: String,
    disableBankAccountDeletion: Boolean,
    externalToPaymentAccountTag: String,
    akkaNodeRole: String
  )
}
