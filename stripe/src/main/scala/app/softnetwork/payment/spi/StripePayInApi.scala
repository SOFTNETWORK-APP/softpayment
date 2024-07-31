package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{
  PayInWithCardPreAuthorizedTransaction,
  PayInWithCardTransaction,
  PayInWithPayPalTransaction,
  Transaction
}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.PaymentIntent
import com.stripe.param.{
  PaymentIntentCaptureParams,
  PaymentIntentCreateParams,
  PaymentIntentUpdateParams
}

import scala.util.{Failure, Success, Try}
import collection.JavaConverters._

trait StripePayInApi extends PayInApi { _: StripeContext =>

  /** @param payInWithCardPreAuthorizedTransaction
    *   - card pre authorized pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with card pre authorized transaction result
    */
  private[spi] override def payInWithCardPreAuthorized(
    payInWithCardPreAuthorizedTransaction: Option[PayInWithCardPreAuthorizedTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    payInWithCardPreAuthorizedTransaction match {
      case Some(payInWithCardPreAuthorizedTransaction) =>
        Try {
          mlog.info(
            s"Capturing payment intent for order: ${payInWithCardPreAuthorizedTransaction.orderUuid}"
          )

          val requestOptions = StripeApi().requestOptions

          var resource = PaymentIntent
            .retrieve(
              payInWithCardPreAuthorizedTransaction.cardPreAuthorizedTransactionId,
              requestOptions
            )

          // optionally update fees amount
          resource =
            Option(resource.getTransferData).flatMap(td => Option(td.getDestination)) match {
              case Some(_) =>
                payInWithCardPreAuthorizedTransaction.feesAmount match {
                  case Some(feesAmount)
                      if feesAmount != Option(resource.getApplicationFeeAmount)
                        .map(_.intValue())
                        .getOrElse(0) =>
                    resource.update(
                      PaymentIntentUpdateParams
                        .builder()
                        .setApplicationFeeAmount(feesAmount)
                        .build(),
                      requestOptions
                    )
                  case _ => resource
                }
              case _ => resource
            }

          resource.getStatus match {
            case "requires_capture" =>
              val params =
                PaymentIntentCaptureParams
                  .builder()
                  .setAmountToCapture(
                    Math.min(
                      resource.getAmountCapturable.intValue(),
                      payInWithCardPreAuthorizedTransaction.debitedAmount
                    )
                  )
                  .putMetadata("transaction_type", "pay_in")
                  .putMetadata("payment_type", "card")
                  .putMetadata(
                    "pre_authorization_id",
                    payInWithCardPreAuthorizedTransaction.cardPreAuthorizedTransactionId
                  )
                  .putMetadata("order_uuid", payInWithCardPreAuthorizedTransaction.orderUuid)
              /*.setTransferData( // transfer destination has to be set in the payment intent creation
                PaymentIntentCaptureParams.TransferData
                  .builder()
                  .setAmount(payInWithCardPreAuthorizedTransaction.debitedAmount)
                  .build()
              )*/

              payInWithCardPreAuthorizedTransaction.statementDescriptor match {
                case Some(statementDescriptor) =>
                  params.setStatementDescriptor(statementDescriptor)
                case _ =>
              }

              resource.capture(
                params.build(),
                requestOptions
              )
            case _ => resource
          }
        } match {
          case Success(payment) =>
            val status = payment.getStatus
            var transaction =
              Transaction()
                .withId(payment.getId)
                .withOrderUuid(payInWithCardPreAuthorizedTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.PAYIN)
                .withAmount(payment.getAmountReceived.intValue())
                .withFees(Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0))
                .withResultCode(status)
                .withPaymentType(Transaction.PaymentType.PREAUTHORIZED)
                .withPreAuthorizationId(
                  payInWithCardPreAuthorizedTransaction.cardPreAuthorizedTransactionId
                )
                .withPreAuthorizationDebitedAmount(
                  payment.getAmountCapturable
                    .intValue() //payInWithCardPreAuthorizedTransaction.preAuthorizationDebitedAmount
                )
                .withCardId(payment.getPaymentMethod)
                .withAuthorId(payment.getCustomer)
                //            .withCreditedUserId(
                //              payInWithCardPreAuthorizedTransaction.creditedWalletId
                //            ) // the credited wallet id is not the user id
                .withCreditedWalletId(payInWithCardPreAuthorizedTransaction.creditedWalletId)
                .withSourceTransactionId(
                  payInWithCardPreAuthorizedTransaction.cardPreAuthorizedTransactionId
                )
                .copy(
                  creditedUserId =
                    Option(payment.getTransferData).flatMap(td => Option(td.getDestination))
                )
            if (status == "succeeded") {
              transaction =
                transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
            } else {
              transaction = transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_FAILED)
            }

            mlog.info(
              s"Payment intent captured for order: ${payInWithCardPreAuthorizedTransaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)
          case Failure(f) =>
            mlog.error(s"Failed to capture payment intent: ${f.getMessage}")
            None
        }
      case _ => None
    }
  }

  /** @param maybePayInTransaction
    *   - pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in transaction result
    */
  private[spi] override def payInWithCard(
    maybePayInTransaction: Option[PayInWithCardTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    maybePayInTransaction match {
      case Some(payInTransaction) =>
        Try {
          val requestOptions = StripeApi().requestOptions

          val params =
            PaymentIntentCreateParams
              .builder()
              .setAmount(payInTransaction.debitedAmount)
              .setCurrency(payInTransaction.currency)
              .setCustomer(payInTransaction.authorId)

              /*.setTransferData(
                PaymentIntentCreateParams.TransferData.builder().setDestination(payInTransaction.creditedWalletId).build()
              )*/
              .setCaptureMethod(PaymentIntentCreateParams.CaptureMethod.MANUAL)
              //.setOffSession(true) // For off-session payments
              .setStatementDescriptorSuffix(payInTransaction.statementDescriptor)
              .setTransferGroup(payInTransaction.orderUuid)
              .putMetadata("order_uuid", payInTransaction.orderUuid)
              .putMetadata("transaction_type", "pay_in")
              .putMetadata("payment_type", "card")
              .putMetadata("print_receipt", payInTransaction.printReceipt.getOrElse(false).toString)
              .putMetadata("register_card", payInTransaction.registerCard.getOrElse(false).toString)

          Option(payInTransaction.cardId) match {
            case Some(cardId) =>
              params
                .setPaymentMethod(cardId)
                .setConfirm(true) // Confirm the PaymentIntent immediately
                .setReturnUrl(
                  s"${config.payInReturnUrl}/${payInTransaction.orderUuid}?transactionIdParameter=payment_intent&registerCard=${payInTransaction.registerCard
                    .getOrElse(false)}&printReceipt=${payInTransaction.printReceipt.getOrElse(false)}"
                )
            case _ =>
              params
                .addPaymentMethodType("card")
                .setPaymentMethodOptions(
                  PaymentIntentCreateParams.PaymentMethodOptions
                    .builder()
                    .setCard(
                      PaymentIntentCreateParams.PaymentMethodOptions.Card
                        .builder()
                        .setCaptureMethod(
                          PaymentIntentCreateParams.PaymentMethodOptions.Card.CaptureMethod.MANUAL
                        )
                        .setRequestThreeDSecure(
                          PaymentIntentCreateParams.PaymentMethodOptions.Card.RequestThreeDSecure.AUTOMATIC
                        )
                        .build()
                    )
                    .build()
                )
          }

          payInTransaction.registerCard match {
            case Some(true) =>
              params.setSetupFutureUsage(PaymentIntentCreateParams.SetupFutureUsage.OFF_SESSION)
            case _ =>
          }

          payInTransaction.ipAddress match {
            case Some(ipAddress) =>
              var onlineParams =
                PaymentIntentCreateParams.MandateData.CustomerAcceptance.Online
                  .builder()
                  .setIpAddress(ipAddress)

              payInTransaction.browserInfo.map(_.userAgent) match {
                case Some(userAgent) =>
                  onlineParams = onlineParams.setUserAgent(userAgent)
                case _ =>
              }

              params
                .setMandateData(
                  PaymentIntentCreateParams.MandateData
                    .builder()
                    .setCustomerAcceptance(
                      PaymentIntentCreateParams.MandateData.CustomerAcceptance
                        .builder()
                        .setAcceptedAt((System.currentTimeMillis() / 1000).toInt)
                        .setOnline(onlineParams.build())
                        .build()
                    )
                    .build()
                )

            case _ =>

          }

          mlog.info(
            s"Creating pay in for order ${payInTransaction.orderUuid} -> ${new Gson()
              .toJson(params.build())}"
          )

          val resource = PaymentIntent.create(params.build(), requestOptions)

          resource.getStatus match {
            case "requires_capture" => // we capture the funds now
              resource.capture(
                PaymentIntentCaptureParams
                  .builder()
                  .setAmountToCapture(
                    Math.min(resource.getAmountCapturable, payInTransaction.debitedAmount)
                  )
                  .build(),
                requestOptions
              )
            case _ => resource
          }
        } match {
          case Success(payment) =>
            val status = payment.getStatus
            var transaction =
              Transaction()
                .withId(payment.getId)
                .withOrderUuid(payInTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.PAYIN)
                .withPaymentType(Transaction.PaymentType.CARD)
                .withAmount(payment.getAmountReceived.intValue())
                .withFees(0)
                .withCurrency(payment.getCurrency)
                .withResultCode(status)
                .withAuthorId(payment.getCustomer)
                .copy(
                  cardId = Option(payment.getPaymentMethod),
                  creditedWalletId = Some(payInTransaction.creditedWalletId)
                )

            if (status == "requires_action" && payment.getNextAction.getType == "redirect_to_url") {
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_CREATED,
                //The URL you must redirect your customer to in order to authenticate the payment.
                redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl)
              )
            } else if (status == "requires_payment_method") {
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
                paymentClientSecret = Option(payment.getClientSecret),
                paymentClientReturnUrl = Option(
                  s"${config.payInReturnUrl}/${payInTransaction.orderUuid}?transactionIdParameter=payment_intent&registerCard=${payInTransaction.registerCard
                    .getOrElse(false)}&printReceipt=${payInTransaction.printReceipt
                    .getOrElse(false)}&payment_intent=${payment.getId}"
                )
              )
            } else if (status == "succeeded" || status == "requires_capture") {
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
            } else {
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
            }

            mlog.info(
              s"Pay in created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)
          case Failure(f) =>
            mlog.error(s"Failed to create pay in: ${f.getMessage}", f)
            None
        }
      case _ => None
    }
  }

  /** @param payInWithPayPalTransaction
    *   - pay in with PayPal transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with PayPal transaction result
    */
  private[spi] override def payInWithPayPal(
    payInWithPayPalTransaction: Option[PayInWithPayPalTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    payInWithPayPalTransaction match {
      case Some(payInTransaction) =>
        Try {
          val requestOptions = StripeApi().requestOptions

          val params =
            PaymentIntentCreateParams
              .builder()
              .setAmount(payInTransaction.debitedAmount)
              .setCurrency(payInTransaction.currency)
              .addPaymentMethodType("paypal")
              .setPaymentMethodOptions(
                PaymentIntentCreateParams.PaymentMethodOptions
                  .builder()
                  .setPaypal(
                    PaymentIntentCreateParams.PaymentMethodOptions.Paypal
                      .builder()
                      .setCaptureMethod(
                        PaymentIntentCreateParams.PaymentMethodOptions.Paypal.CaptureMethod.MANUAL
                      )
                      .build()
                  )
                  .build()
              )
              .setPaymentMethodData(
                PaymentIntentCreateParams.PaymentMethodData
                  .builder()
                  .setType(PaymentIntentCreateParams.PaymentMethodData.Type.PAYPAL)
                  .setPaypal(
                    PaymentIntentCreateParams.PaymentMethodData.Paypal.builder().build()
                  )
                  .build()
              )
              .setCustomer(payInTransaction.authorId)
              .setCaptureMethod(PaymentIntentCreateParams.CaptureMethod.MANUAL)
              .setTransferGroup(payInTransaction.orderUuid)
              .putMetadata("order_uuid", payInTransaction.orderUuid)
              .putMetadata("transaction_type", "pay_in")
              .putMetadata("payment_type", "paypal")
              .putMetadata(
                "print_receipt",
                payInTransaction.printReceipt.getOrElse(false).toString
              )

          payInTransaction.statementDescriptor match {
            case Some(statementDescriptor) =>
              params.setStatementDescriptor(statementDescriptor)
            case _ =>
          }

          payInTransaction.ipAddress match {
            case Some(ipAddress) =>
              var onlineParams =
                PaymentIntentCreateParams.MandateData.CustomerAcceptance.Online
                  .builder()
                  .setIpAddress(ipAddress)

              payInTransaction.browserInfo.map(_.userAgent) match {
                case Some(userAgent) =>
                  onlineParams = onlineParams.setUserAgent(userAgent)
                case _ =>
              }

              params
                //.setOffSession(true) // For off-session payments
                .setMandateData(
                  PaymentIntentCreateParams.MandateData
                    .builder()
                    .setCustomerAcceptance(
                      PaymentIntentCreateParams.MandateData.CustomerAcceptance
                        .builder()
                        .setAcceptedAt((System.currentTimeMillis() / 1000).toInt)
                        .setOnline(onlineParams.build())
                        .build()
                    )
                    .build()
                )

            case _ =>

          }

          payInTransaction.payPalWalletId match {
            case Some(payPalWalletId) =>
              params
                .setConfirm(true) // Confirm the PaymentIntent immediately
                .setPaymentMethod(payPalWalletId)
                .setReturnUrl(
                  s"${config.payInReturnUrl}/${payInTransaction.orderUuid}?transactionIdParameter=payment_intent&registerCard=false&&printReceipt=${payInTransaction.printReceipt
                    .getOrElse(false)}"
                )
            case _ =>
          }

          mlog.info(
            s"Creating pay in with PayPal for order ${payInTransaction.orderUuid} -> ${new Gson()
              .toJson(params.build())}"
          )

          val resource = PaymentIntent.create(params.build(), requestOptions)

          resource.getStatus match {
            case "requires_capture" => // we capture the funds now
              resource.capture(
                PaymentIntentCaptureParams
                  .builder()
                  .setAmountToCapture(
                    Math.min(resource.getAmountCapturable, payInTransaction.debitedAmount)
                  )
                  .build(),
                requestOptions
              )
            case _ => resource
          }
        } match {
          case Success(payment) =>
            mlog.info(s"Pay in with PayPal -> ${new Gson().toJson(payment)}")

            val status = payment.getStatus
            var transaction =
              Transaction()
                .withId(payment.getId)
                .withOrderUuid(payInTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.PAYIN)
                .withPaymentType(Transaction.PaymentType.PAYPAL)
                .withAmount(payment.getAmountReceived.intValue())
                .withFees(0)
                .withCurrency(payment.getCurrency)
                .withResultCode(status)
                .withAuthorId(payment.getCustomer)
                .copy(
                  creditedWalletId = Some(payInTransaction.creditedWalletId)
                )

            if (status == "requires_action" && payment.getNextAction.getType == "redirect_to_url") {
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_CREATED,
                //The URL you must redirect your customer to in order to authenticate the payment.
                redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl),
                returnUrl = Option(payment.getNextAction.getRedirectToUrl.getReturnUrl)
              )
            } else if (status == "requires_payment_method" || status == "requires_confirmation") {
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
                paymentClientSecret = Option(payment.getClientSecret),
                paymentClientReturnUrl = Option(
                  s"${config.payInReturnUrl}/${payInTransaction.orderUuid}?transactionIdParameter=payment_intent&registerCard=false&printReceipt=${payInTransaction.printReceipt
                    .getOrElse(false)}&payment_intent=${payment.getId}"
                )
              )
            } else if (status == "succeeded" || status == "requires_capture") {
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
            } else {
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
            }

            mlog.info(
              s"Pay in with PayPal created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)
          case Failure(f) =>
            mlog.error(s"Failed to create pay in with PayPal: ${f.getMessage}", f)
            None
        }
      case _ => None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay in transaction
    */
  override def loadPayInTransaction(
    orderUuid: String,
    transactionId: String,
    recurringPayInRegistrationId: Option[String]
  ): Option[Transaction] = {
    Try {
      PaymentIntent.retrieve(transactionId, StripeApi().requestOptions)
    } match {
      case Success(payment) =>
        val status = payment.getStatus
        val metadata = payment.getMetadata.asScala
        val `type` =
          metadata.get("transaction_type") match {
            case Some("pre_authorization") => Transaction.TransactionType.PRE_AUTHORIZATION
            case Some("direct_debit")      => Transaction.TransactionType.DIRECT_DEBIT
            case _                         => Transaction.TransactionType.PAYIN
          }
        val paymentType =
          metadata.get("payment_type") match {
            case Some("paypal") => Transaction.PaymentType.PAYPAL
            case _              => Transaction.PaymentType.CARD
          }
        val cardId =
          paymentType match {
            case Transaction.PaymentType.CARD => Option(payment.getPaymentMethod)
            case _                            => None
          }
        val preAuthorizationId = metadata.get("pre_authorization_id")
        val redirectUrl =
          if (status == "requires_action" && payment.getNextAction.getType == "redirect_to_url") {
            Option(payment.getNextAction.getRedirectToUrl.getUrl)
          } else {
            None
          }

        var transaction =
          Transaction()
            .withId(payment.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(`type`)
            .withPaymentType(paymentType)
            .withAmount(payment.getAmount.intValue())
            .withFees(Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0))
            .withResultCode(status)
            .withAuthorId(payment.getCustomer)
            .withCurrency(payment.getCurrency)
            .copy(
              cardId = cardId,
              preAuthorizationId = preAuthorizationId,
              redirectUrl = redirectUrl
            )

        if (status == "requires_action" && payment.getNextAction.getType == "redirect_to_url") {
          transaction = transaction.copy(
            status = Transaction.TransactionStatus.TRANSACTION_CREATED,
            //The URL you must redirect your customer to in order to authenticate the payment.
            redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl)
          )
        } else if (status == "requires_payment_method" || status == "requires_confirmation") {
          transaction = transaction.copy(
            status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
            paymentClientSecret = Option(payment.getClientSecret),
            paymentClientReturnUrl = Option(
              s"${config.payInReturnUrl}/$orderUuid?transactionIdParameter=payment_intent&registerCard=${metadata
                .getOrElse("register_card", false)}&printReceipt=${metadata
                .getOrElse("print_receipt", false)}&payment_intent=${payment.getId}"
            )
          )
        } else if (status == "succeeded" || status == "requires_capture") {
          transaction =
            transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
        } else {
          transaction = transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
        }

        mlog.info(
          s"Pay in with PayPal created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
        )

        Some(transaction)
      case Failure(f) =>
        mlog.error(s"Failed to load pay in transaction: ${f.getMessage}", f)
        None
    }
  }
}
