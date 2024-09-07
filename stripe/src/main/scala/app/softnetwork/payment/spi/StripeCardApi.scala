package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{
  Card,
  CardPreRegistration,
  PreAuthorizationTransaction,
  Transaction
}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{Customer, PaymentIntent, PaymentMethod, SetupIntent}
import com.stripe.param.{
  PaymentIntentCancelParams,
  PaymentIntentConfirmParams,
  PaymentIntentCreateParams,
  PaymentMethodAttachParams,
  PaymentMethodDetachParams,
  SetupIntentCreateParams
}

import scala.util.{Failure, Success, Try}

trait StripeCardApi extends CardApi { _: StripeContext =>

  /** @param maybeUserId
    *   - owner of the card
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @return
    *   card pre registration
    */
  override def preRegisterCard(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String
  ): Option[CardPreRegistration] = {
    maybeUserId match {
      case Some(userId) =>
        Try {
          val customer =
            Customer.retrieve(userId, StripeApi().requestOptions)

          val params =
            SetupIntentCreateParams
              .builder()
              .addPaymentMethodType("card")
              .setCustomer(customer.getId)
              .setUsage(SetupIntentCreateParams.Usage.ON_SESSION)
              .setPaymentMethodOptions(
                SetupIntentCreateParams.PaymentMethodOptions
                  .builder()
                  .setCard(
                    SetupIntentCreateParams.PaymentMethodOptions.Card
                      .builder()
                      .setRequestThreeDSecure(
                        SetupIntentCreateParams.PaymentMethodOptions.Card.RequestThreeDSecure.ANY
                      )
                      .build()
                  )
                  .build()
              )
//              .setUseStripeSdk(false)
              .addFlowDirection(SetupIntentCreateParams.FlowDirection.INBOUND)
              .putMetadata("currency", currency)
              .putMetadata("external_uuid", externalUuid)
          // TODO check if return url is required

          SetupIntent.create(params.build(), StripeApi().requestOptions)
        } match {
          case Success(setupIntent) =>
            mlog.info(s"Card pre registered for user $userId -> ${new Gson().toJson(setupIntent)}")
            Some(
              CardPreRegistration.defaultInstance
                .withId(setupIntent.getId)
                .withAccessKey(setupIntent.getClientSecret)
            )
          case Failure(f) =>
            mlog.error(s"Error while pre registering card for user $userId", f)
            None
        }
      case _ => None
    }
  }

  /** @param cardPreRegistrationId
    *   - card registration id
    * @param maybeRegistrationData
    *   - card registration data
    * @return
    *   card id
    */
  override def createCard(
    cardPreRegistrationId: String,
    maybeRegistrationData: Option[String]
  ): Option[String] = {
    Try {
      // retrieve setup intent
      val setupIntent =
        SetupIntent.retrieve(cardPreRegistrationId, StripeApi().requestOptions)
      // attach payment method to customer
      PaymentMethod
        .retrieve(setupIntent.getPaymentMethod, StripeApi().requestOptions)
        .attach(
          PaymentMethodAttachParams.builder().setCustomer(setupIntent.getCustomer).build(),
          StripeApi().requestOptions
        )
    } match {
      case Success(paymentMethod) =>
        mlog.info(s"Card ${paymentMethod.getId} attached to customer ${paymentMethod.getCustomer}")
        Some(paymentMethod.getId)
      case Failure(f) =>
        mlog.error(s"Error while creating card for $cardPreRegistrationId", f)
        None
    }
  }

  /** @param cardId
    *   - card id
    * @return
    *   card
    */
  override def loadCard(cardId: String): Option[Card] = {
    Try {
      PaymentMethod.retrieve(cardId, StripeApi().requestOptions)
    } match {
      case Success(paymentMethod) =>
        Option(paymentMethod.getCard) match {
          case Some(card) =>
            Some(
              Card.defaultInstance
                .withId(cardId)
                .withExpirationDate(s"${card.getExpMonth}/${card.getExpYear}")
                .withAlias(card.getLast4)
                .withBrand(card.getBrand)
                .withActive(
                  Option(paymentMethod.getCustomer).isDefined
                ) // if detached from customer, it is disabled
            )
          case _ => None
        }
      case Failure(f) =>
        mlog.error(s"Error while loading card $cardId", f)
        None
    }
  }

  /** @param cardId
    *   - the id of the card to disable
    * @return
    *   the card disabled or none
    */
  override def disableCard(cardId: String): Option[Card] = {
    Try {
      val requestOptions = StripeApi().requestOptions
      PaymentMethod
        .retrieve(cardId, requestOptions)
        .detach(PaymentMethodDetachParams.builder().build(), requestOptions)
    } match {
      case Success(paymentMethod) =>
        mlog.info(s"Card $cardId detached from customer ${paymentMethod.getCustomer}")
        val card = paymentMethod.getCard
        Some(
          Card.defaultInstance
            .withId(cardId)
            .withExpirationDate(s"${card.getExpMonth}/${card.getExpYear}")
            .withAlias(card.getLast4)
            .withBrand(card.getBrand)
            .withActive(false)
        )
      case Failure(f) =>
        mlog.error(s"Error while disabling card $cardId", f)
        None
    }
  }

  /** @param preAuthorizationTransaction
    *   - pre authorization transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pre authorization transaction result
    */
  override def preAuthorizeCard(
    preAuthorizationTransaction: PreAuthorizationTransaction,
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    Try {
      val mayRequire3DS =
        preAuthorizationTransaction.preRegistrationId match {
          case Some(preRegistrationId) =>
            val setup = SetupIntent.retrieve(preRegistrationId, StripeApi().requestOptions)
            mlog.info(
              s"Setup intent retrieved for order ${preAuthorizationTransaction.orderUuid} -> ${new Gson()
                .toJson(setup)}"
            )
            setup.getStatus match {
              case "succeeded" => false
              case _           => true
            }
          case _ => true
        }
      val params =
        PaymentIntentCreateParams
          .builder()
          .setAmount(preAuthorizationTransaction.debitedAmount)
          .setCurrency(preAuthorizationTransaction.currency)
          .setCustomer(preAuthorizationTransaction.authorId)
          .setPaymentMethod(preAuthorizationTransaction.cardId)
          .setPaymentMethodOptions(
            PaymentIntentCreateParams.PaymentMethodOptions
              .builder()
              .setCard(
                PaymentIntentCreateParams.PaymentMethodOptions.Card
                  .builder()
                  .setRequestThreeDSecure(
                    PaymentIntentCreateParams.PaymentMethodOptions.Card.RequestThreeDSecure.AUTOMATIC
                  )
                  .build()
              )
              .build()
          )
          .setCaptureMethod(
            PaymentIntentCreateParams.CaptureMethod.MANUAL
          ) // To capture funds later (https://stripe.com/docs/payments/capture-later)
          .setConfirm(true) // Confirm the PaymentIntent immediately
          //.setOffSession(true) // For off-session payments
          .setReturnUrl(
            s"${config.preAuthorizeCardReturnUrl}/${preAuthorizationTransaction.orderUuid}?preAuthorizationIdParameter=payment_intent&registerCard=${preAuthorizationTransaction.registerCard
              .getOrElse(false)}&printReceipt=${preAuthorizationTransaction.printReceipt.getOrElse(false)}"
          )
          .setSetupFutureUsage(
            PaymentIntentCreateParams.SetupFutureUsage.OFF_SESSION
          ) // For off-session payments
          .setTransferGroup(preAuthorizationTransaction.orderUuid)
          .putMetadata("order_uuid", preAuthorizationTransaction.orderUuid)
          .putMetadata("transaction_type", "pre_authorization")
          .putMetadata("payment_type", "card")

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
            PaymentIntentCreateParams.TransferData.builder().setDestination(creditedUserId).build()
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
        s"Creating card pre authorization for order ${preAuthorizationTransaction.orderUuid} -> ${new Gson()
          .toJson(params.build())}"
      )
      (PaymentIntent.create(params.build(), StripeApi().requestOptions), mayRequire3DS)
    } match {
      case Success((paymentIntent, mayRequired3DS)) =>
        mlog.info(
          s"Card pre authorization created for order ${preAuthorizationTransaction.orderUuid} -> ${new Gson()
            .toJson(paymentIntent)}"
        )
        val status = paymentIntent.getStatus
        var transaction =
          Transaction()
            .withId(paymentIntent.getId)
            .withOrderUuid(preAuthorizationTransaction.orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.PRE_AUTHORIZATION)
            .withAmount(preAuthorizationTransaction.debitedAmount)
            .withCardId(preAuthorizationTransaction.cardId)
            .withFees(0)
            .withResultCode(status)
            .withAuthorId(paymentIntent.getCustomer)
            .withPaymentType(Transaction.PaymentType.CARD)
            .withCurrency(paymentIntent.getCurrency)
            .copy(
              creditedUserId = preAuthorizationTransaction.creditedUserId,
              fees = preAuthorizationTransaction.feesAmount.getOrElse(0),
              preRegistrationId = preAuthorizationTransaction.preRegistrationId
            )

        status match {
          case "requires_action"
              if paymentIntent.getNextAction.getType == "redirect_to_url" /*&& mayRequired3DS*/ =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_CREATED,
              //The URL you must redirect your customer to in order to authenticate the payment.
              redirectUrl = Option(paymentIntent.getNextAction.getRedirectToUrl.getUrl),
              returnUrl = Option(paymentIntent.getNextAction.getRedirectToUrl.getReturnUrl)
            )
          case "requires_payment_method" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
          case "succeeded" | "requires_capture" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
          case _ =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
        }

        mlog.info(
          s"Card pre authorization created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
        )

        Some(transaction)

      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   card pre authorized transaction
    */
  override def loadCardPreAuthorized(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Option[Transaction] = {
    Try {
      PaymentIntent.retrieve(cardPreAuthorizedTransactionId, StripeApi().requestOptions)
    } match {
      case Success(paymentIntent) =>
        val status = paymentIntent.getStatus
        var transaction =
          Transaction()
            .withId(paymentIntent.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.PRE_AUTHORIZATION)
            .withAmount(paymentIntent.getAmount.intValue())
            .withCardId(paymentIntent.getPaymentMethod)
            .withFees(0)
            .withResultCode(status)
            .withAuthorId(paymentIntent.getCustomer)
            .withPaymentType(Transaction.PaymentType.CARD)
            .withCurrency(paymentIntent.getCurrency)

        if (
          status == "requires_action" && paymentIntent.getNextAction.getType == "redirect_to_url"
        ) {
          transaction = transaction.copy(
            status = Transaction.TransactionStatus.TRANSACTION_CREATED,
            //The URL you must redirect your customer to in order to authenticate the payment.
            returnUrl = Option(paymentIntent.getNextAction.getRedirectToUrl.getUrl)
          )
        } else if (status == "succeeded") {
          transaction =
            transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
        } else if (status == "requires_payment_method") {
          transaction = transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
        } else {
          transaction = transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
        }

        // TODO retrieve preAuthorizationCanceled, preAuthorizationValidated, preAuthorizationExpired
        Some(transaction)

      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been cancelled or not
    */
  override def cancelPreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Boolean = {
    Try {
      PaymentIntent
        .retrieve(cardPreAuthorizedTransactionId, StripeApi().requestOptions)
        .cancel(PaymentIntentCancelParams.builder().build(), StripeApi().requestOptions)
    } match {
      case Success(_) =>
        mlog.info(s"Card pre authorization canceled for order -> $orderUuid")
        true
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        false
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been validated or not
    */
  override def validatePreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Boolean = {
    Try {
      val resource =
        PaymentIntent
          .retrieve(cardPreAuthorizedTransactionId, StripeApi().requestOptions)
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
            mlog.info(s"Card pre authorization validated for order -> $orderUuid")
            true
          case _ => false
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        false
    }
  }
}
