package app.softnetwork.payment.model

trait PayInTransactionDecorator { _: PayInTransaction =>

  def cardTransaction: Option[PayInWithCardTransaction] =
    paymentType match {
      case Transaction.PaymentType.CARD =>
        Some(
          PayInWithCardTransaction.defaultInstance
            .withOrderUuid(orderUuid)
            .withAuthorId(authorId)
            .withDebitedAmount(debitedAmount)
            .withFeesAmount(feesAmount)
            .withCurrency(currency)
            .withCreditedWalletId(creditedWalletId)
            .withStatementDescriptor(statementDescriptor)
            .withCardId(paymentMethodId.orNull)
            .copy(
              browserInfo = browserInfo,
              ipAddress = ipAddress,
              registerCard = registerMeansOfPayment,
              printReceipt = printReceipt,
              preRegistrationId = preRegistrationId
            )
        )
      case _ => None
    }

  def preAuthorizationTransaction: Option[PayInWithPreAuthorization] =
    paymentType match {
      case Transaction.PaymentType.PREAUTHORIZED =>
        Some(
          PayInWithPreAuthorization.defaultInstance
            .withOrderUuid(orderUuid)
            .withAuthorId(authorId)
            .withDebitedAmount(debitedAmount)
            .withFeesAmount(feesAmount)
            .withCurrency(currency)
            .withCreditedWalletId(creditedWalletId)
            .withPreAuthorizedTransactionId(preAuthorizedTransactionId.orNull)
            .withPreAuthorizationDebitedAmount(
              preAuthorizationDebitedAmount.getOrElse(debitedAmount)
            )
            .copy(
              statementDescriptor =
                if (statementDescriptor.isEmpty) None else Some(statementDescriptor),
              preRegistrationId = preRegistrationId
            )
        )
      case _ => None
    }

  def payPalTransaction: Option[PayInWithPayPalTransaction] =
    paymentType match {
      case Transaction.PaymentType.PAYPAL =>
        Some(
          PayInWithPayPalTransaction.defaultInstance
            .withOrderUuid(orderUuid)
            .withAuthorId(authorId)
            .withDebitedAmount(debitedAmount)
            .withFeesAmount(feesAmount)
            .withCurrency(currency)
            .withCreditedWalletId(creditedWalletId)
            .withStatementDescriptor(statementDescriptor)
            .withPaypalId(paymentMethodId.orNull)
            .copy(
              browserInfo = browserInfo,
              ipAddress = ipAddress,
              registerPaypal = registerMeansOfPayment,
              printReceipt = printReceipt,
              language = browserInfo.map(_.language),
              preRegistrationId = preRegistrationId
            )
        )
      case _ => None
    }
}
