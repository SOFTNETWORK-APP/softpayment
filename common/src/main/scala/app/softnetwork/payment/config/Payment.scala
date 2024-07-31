package app.softnetwork.payment.config

object Payment {

  case class Config(
    baseUrl: String,
    path: String,
    payInRoute: String,
    payInStatementDescriptor: String,
    preAuthorizeCardRoute: String,
    recurringPaymentRoute: String,
    callbacksRoute: String,
    hooksRoute: String,
    mandateRoute: String,
    cardRoute: String,
    bankRoute: String,
    declarationRoute: String,
    kycRoute: String,
    disableBankAccountDeletion: Boolean,
    externalToPaymentAccountTag: String,
    akkaNodeRole: String
  )
}
