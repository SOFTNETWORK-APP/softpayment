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
            .withCardId(cardId.orNull)
            .copy(
              browserInfo = browserInfo,
              ipAddress = ipAddress,
              registerCard = registerCard,
              printReceipt = printReceipt
            )
        )
      case _ => None
    }

  def cardPreAuthorizedTransaction: Option[PayInWithCardPreAuthorizedTransaction] =
    paymentType match {
      case Transaction.PaymentType.PREAUTHORIZED =>
        Some(
          PayInWithCardPreAuthorizedTransaction.defaultInstance
            .withOrderUuid(orderUuid)
            .withAuthorId(authorId)
            .withDebitedAmount(debitedAmount)
            .withFeesAmount(feesAmount)
            .withCurrency(currency)
            .withCreditedWalletId(creditedWalletId)
            .withCardPreAuthorizedTransactionId(cardPreAuthorizedTransactionId.orNull)
            .withPreAuthorizationDebitedAmount(
              preAuthorizationDebitedAmount.getOrElse(debitedAmount)
            )
            .copy(
              statementDescriptor =
                if (statementDescriptor.isEmpty) None else Some(statementDescriptor)
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
            .copy(
              browserInfo = browserInfo,
              ipAddress = ipAddress,
              printReceipt = printReceipt,
              language = browserInfo.map(_.language)
            )
        )
      case _ => None
    }
}
