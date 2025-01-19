package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{PreAuthorizationTransaction, Transaction}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{PaymentIntent, SetupIntent}
import com.stripe.param.{
  PaymentIntentCancelParams,
  PaymentIntentConfirmParams,
  PaymentIntentCreateParams
}

import scala.util.{Failure, Success, Try}

trait StripePreAuthorizationApi extends PreAuthorizationApi { _: StripeContext =>

  /** @param preAuthorizationTransaction
    *   - pre authorization
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pre authorized transaction result
    */
  override def preAuthorize(
    preAuthorizationTransaction: PreAuthorizationTransaction,
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    val paymentType = preAuthorizationTransaction.paymentType
    if (paymentType.isCard || paymentType.isPaypal) {
      val requestOptions = StripeApi().requestOptions
      val registerMeansOfPayment =
        preAuthorizationTransaction.registerMeansOfPayment.getOrElse(false)
      val printReceipt = preAuthorizationTransaction.printReceipt.getOrElse(false)
      Try {
        val params =
          PaymentIntentCreateParams
            .builder()
            .setAmount(preAuthorizationTransaction.debitedAmount)
            .setCurrency(preAuthorizationTransaction.currency)
            .setCustomer(preAuthorizationTransaction.authorId)
            .setCaptureMethod(
              PaymentIntentCreateParams.CaptureMethod.MANUAL
            ) // To capture funds later (https://stripe.com/docs/payments/capture-later)
            .setTransferGroup(preAuthorizationTransaction.orderUuid)
            .putMetadata("order_uuid", preAuthorizationTransaction.orderUuid)
            .putMetadata("transaction_type", "pre_authorization")
            .putMetadata("payment_type", paymentType.name.toLowerCase)
            .putMetadata("register_means_of_payment", s"$registerMeansOfPayment")
            .putMetadata("print_receipt", s"$printReceipt")

        preAuthorizationTransaction.preRegistrationId match {
          case Some(preRegistrationId) =>
            params.putMetadata("pre_registration_id", preRegistrationId)
          case _ =>
        }

        (preAuthorizationTransaction.paymentMethodId match {
          case None =>
            preAuthorizationTransaction.preRegistrationId match {
              case Some(preRegistrationId) =>
                val setup = SetupIntent.retrieve(preRegistrationId, requestOptions)
                mlog.info(
                  s"Setup intent retrieved for order ${preAuthorizationTransaction.orderUuid} -> ${new Gson()
                    .toJson(setup)}"
                )
                setup.getStatus match {
                  case "succeeded" =>
                    Option(setup.getPaymentMethod)
                  case _ =>
                    None
                }
              case _ =>
                None
            }
          case some => some
        }) match {
          case Some(paymentMethodId) =>
            params
              .setPaymentMethod(paymentMethodId)
              .setConfirm(true) // Confirm the PaymentIntent immediately
              .setReturnUrl(
                s"${config.preAuthorizeReturnUrl}/${preAuthorizationTransaction.orderUuid}?preAuthorizationIdParameter=payment_intent&registerMeansOfPayment=$registerMeansOfPayment&printReceipt=$printReceipt"
              )
          case _ =>
            paymentType match {
              case Transaction.PaymentType.PAYPAL =>
                params
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
              case Transaction.PaymentType.CARD =>
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
              case _ =>
            }
        }

        if (registerMeansOfPayment) {
          params.setSetupFutureUsage(
            PaymentIntentCreateParams.SetupFutureUsage.OFF_SESSION
          ) // For off-session payments
        }

        preAuthorizationTransaction.ipAddress match {
          case Some(ipAddress) =>
            var onlineParams =
              PaymentIntentCreateParams.MandateData.CustomerAcceptance.Online
                .builder()
                .setIpAddress(ipAddress)

            preAuthorizationTransaction.browserInfo.map(_.userAgent) match {
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

        preAuthorizationTransaction.creditedUserId match {
          case Some(creditedUserId) =>
            params.setTransferData(
              PaymentIntentCreateParams.TransferData
                .builder()
                .setDestination(creditedUserId)
                .build()
            )
            preAuthorizationTransaction.feesAmount match {
              case Some(feesAmount) =>
                params.setApplicationFeeAmount(feesAmount)
              case _ =>
            }
          case _ =>
        }

        preAuthorizationTransaction.statementDescriptor match {
          case Some(statementDescriptor) =>
            params.setStatementDescriptor(statementDescriptor)
          case _ =>
        }

        mlog.info(
          s"Creating pre authorization for order ${preAuthorizationTransaction.orderUuid} -> ${new Gson()
            .toJson(params.build())}"
        )
        PaymentIntent.create(params.build(), requestOptions)
      } match {
        case Success(payment) =>
          mlog.info(
            s"Pre authorization created for order ${preAuthorizationTransaction.orderUuid} -> ${new Gson()
              .toJson(payment)}"
          )
          val status = payment.getStatus
          var transaction =
            Transaction()
              .withId(payment.getId)
              .withOrderUuid(preAuthorizationTransaction.orderUuid)
              .withNature(Transaction.TransactionNature.REGULAR)
              .withType(Transaction.TransactionType.PRE_AUTHORIZATION)
              .withAmount(payment.getAmount.intValue())
              .withResultCode(status)
              .withAuthorId(payment.getCustomer)
              .withPaymentType(paymentType)
              .withCurrency(payment.getCurrency)
              .copy(
                creditedUserId = preAuthorizationTransaction.creditedUserId,
                fees = Option(payment.getApplicationFeeAmount).map(_.toInt).getOrElse(0),
                preRegistrationId = preAuthorizationTransaction.preRegistrationId,
                paymentMethodId = Option(payment.getPaymentMethod)
              )

          status match {
            case "requires_action" if payment.getNextAction.getType == "redirect_to_url" =>
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_CREATED,
                //The URL you must redirect your customer to in order to authenticate the payment.
                redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl),
                returnUrl = Option(
                  s"${config.preAuthorizeReturnUrl}/${preAuthorizationTransaction.orderUuid}?preAuthorizationIdParameter=payment_intent&registerMeansOfPayment=$registerMeansOfPayment&printReceipt=$printReceipt&payment_intent=${payment.getId}"
                )
              )
            case "requires_payment_method" =>
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
                paymentClientSecret = Option(payment.getClientSecret),
                paymentClientReturnUrl = Option(
                  s"${config.preAuthorizeReturnUrl}/${preAuthorizationTransaction.orderUuid}?preAuthorizationIdParameter=payment_intent&registerMeansOfPayment=$registerMeansOfPayment&printReceipt=$printReceipt&payment_intent=${payment.getId}"
                )
              )
            case "succeeded" | "requires_capture" =>
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
            case _ =>
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
          }

          mlog.info(
            s"Pre authorization transaction created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
          )

          Some(transaction)

        case Failure(f) =>
          mlog.error(f.getMessage, f)
          None
      }
    } else {
      mlog.error(s"Unsupported payment type: $paymentType")
      None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param preAuthorizedTransactionId
    *   - pre authorized transaction id
    * @return
    *   pre authorized transaction
    */
  override def loadPreAuthorization(
    orderUuid: String,
    preAuthorizedTransactionId: String
  ): Option[Transaction] = {
    Try {
      PaymentIntent.retrieve(preAuthorizedTransactionId, StripeApi().requestOptions)
    } match {
      case Success(payment) =>
        val status = payment.getStatus
        val paymentType =
          Option(payment.getMetadata.get("payment_type")) match {
            case Some("paypal") => Transaction.PaymentType.PAYPAL
            case _              => Transaction.PaymentType.CARD
          }
        val registerMeansOfPayment =
          Option(payment.getMetadata.get("register_means_of_payment")).getOrElse(false)
        val printReceipt = Option(payment.getMetadata.get("print_receipt")).getOrElse(false)
        var transaction =
          Transaction()
            .withId(payment.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.PRE_AUTHORIZATION)
            .withAmount(payment.getAmount.intValue())
            .withResultCode(status)
            .withAuthorId(payment.getCustomer)
            .withPaymentType(paymentType)
            .withCurrency(payment.getCurrency)
            .copy(
              creditedUserId = Option(payment.getTransferData).map(_.getDestination),
              fees = Option(payment.getApplicationFeeAmount).map(_.toInt).getOrElse(0),
              preRegistrationId = Option(payment.getMetadata.get("pre_registration_id")),
              paymentMethodId = Option(payment.getPaymentMethod)
            )

        status match {
          case "requires_action" if payment.getNextAction.getType == "redirect_to_url" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_CREATED,
              //The URL you must redirect your customer to in order to authenticate the payment.
              redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl),
              returnUrl = Option(
                s"${config.preAuthorizeReturnUrl}/$orderUuid?preAuthorizationIdParameter=payment_intent&registerMeansOfPayment=$registerMeansOfPayment&printReceipt=$printReceipt&payment_intent=${payment.getId}"
              )
            )
          case "succeeded" | "requires_capture" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
          case "requires_payment_method" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
              paymentClientSecret = Option(payment.getClientSecret),
              paymentClientReturnUrl = Option(
                s"${config.preAuthorizeReturnUrl}/$orderUuid?preAuthorizationIdParameter=payment_intent&registerMeansOfPayment=$registerMeansOfPayment&printReceipt=$printReceipt&payment_intent=${payment.getId}"
              )
            )
          case _ =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
        }

        mlog.info(
          s"Pre authorization loaded for order $orderUuid -> ${asJson(transaction)}"
        )

        // TODO retrieve preAuthorizationCanceled, preAuthorizationValidated, preAuthorizationExpired
        Some(transaction)

      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param preAuthorizedTransactionId
    *   - pre authorized transaction id
    * @return
    *   whether pre authorized transaction has been cancelled or not
    */
  override def cancelPreAuthorization(
    orderUuid: String,
    preAuthorizedTransactionId: String
  ): Boolean = {
    Try {
      val payment = PaymentIntent
        .retrieve(preAuthorizedTransactionId, StripeApi().requestOptions)
      payment.getStatus match {
        case "requires_payment_method" | "requires_capture" | "requires_confirmation" |
            "requires_action" | "processing" =>
          payment.cancel(
            PaymentIntentCancelParams.builder().build(),
            StripeApi().requestOptions
          )
        case _ => payment
      }
    } match {
      case Success(payment) =>
        val canceled = payment.getStatus == "canceled"
        mlog.info(
          s"Pre authorization ${payment.getId} with status ${payment.getStatus} ${if (!canceled) { "not " }}canceled for order -> $orderUuid"
        )
        canceled
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        false
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param preAuthorizedTransactionId
    *   - pre authorized transaction id
    * @return
    *   whether pre authorized transaction has been validated or not
    */
  override def validatePreAuthorization(
    orderUuid: String,
    preAuthorizedTransactionId: String
  ): Boolean = {
    Try {
      val resource =
        PaymentIntent
          .retrieve(preAuthorizedTransactionId, StripeApi().requestOptions)
      resource.getStatus match {
        case "requires_confirmation" =>
          resource.confirm(
            PaymentIntentConfirmParams.builder().build(),
            StripeApi().requestOptions
          )
        case _ => resource
      }
    } match {
      case Success(paymentIntent) =>
        paymentIntent.getStatus match {
          case "succeeded" =>
            mlog.info(s"Pre authorization validated for order -> $orderUuid")
            true
          case _ => false
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        false
    }
  }

}
