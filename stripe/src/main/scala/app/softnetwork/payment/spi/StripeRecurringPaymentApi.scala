package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{RecurringPayment, RecurringPaymentTransaction, Transaction}
import com.stripe.model.{Invoice, PaymentIntent, Product, Subscription}
import com.stripe.net.RequestOptions
import com.stripe.param.{
  InvoiceListParams,
  ProductCreateParams,
  SubscriptionCancelParams,
  SubscriptionCreateParams,
  SubscriptionUpdateParams
}

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

trait StripeRecurringPaymentApi extends RecurringPaymentApi { _: StripeContext =>

  /** Maps RecurringPaymentFrequency to Stripe (interval, interval_count). */
  private[spi] def toStripeInterval(
    frequency: RecurringPayment.RecurringPaymentFrequency
  ): (String, Long) = frequency match {
    case RecurringPayment.RecurringPaymentFrequency.DAILY  => ("day", 1L)
    case RecurringPayment.RecurringPaymentFrequency.WEEKLY => ("week", 1L)
    // Stripe has no native semi-monthly interval; bi-weekly (~26/yr) approximates twice-a-month (24/yr)
    case RecurringPayment.RecurringPaymentFrequency.TWICE_A_MONTH => ("week", 2L)
    case RecurringPayment.RecurringPaymentFrequency.MONTHLY       => ("month", 1L)
    case RecurringPayment.RecurringPaymentFrequency.BIMONTHLY     => ("month", 2L)
    case RecurringPayment.RecurringPaymentFrequency.QUARTERLY     => ("month", 3L)
    case RecurringPayment.RecurringPaymentFrequency.BIANNUAL      => ("month", 6L)
    case RecurringPayment.RecurringPaymentFrequency.ANNUAL        => ("year", 1L)
    case _                                                        => ("month", 1L)
  }

  /** Maps Stripe subscription status to RecurringCardPaymentStatus.
    * @param stripeStatus
    *   Stripe subscription status string
    * @param requiresAction
    *   true if the subscription's latest invoice payment intent has status "requires_action"
    *   (3DS/SCA)
    */
  private[spi] def toRecurringCardPaymentStatus(
    stripeStatus: String,
    requiresAction: Boolean = false
  ): RecurringPayment.RecurringCardPaymentStatus = stripeStatus match {
    case "active" | "trialing" => RecurringPayment.RecurringCardPaymentStatus.IN_PROGRESS
    case "incomplete" if requiresAction =>
      RecurringPayment.RecurringCardPaymentStatus.AUTHENTICATION_NEEDED
    case "incomplete" | "past_due"       => RecurringPayment.RecurringCardPaymentStatus.CREATED
    case "unpaid"                        => RecurringPayment.RecurringCardPaymentStatus.CREATED
    case "canceled"                      => RecurringPayment.RecurringCardPaymentStatus.ENDED
    case "incomplete_expired" | "paused" => RecurringPayment.RecurringCardPaymentStatus.ENDED
    case _                               => RecurringPayment.RecurringCardPaymentStatus.CREATED
  }

  /** Check if a subscription's latest invoice payment intent requires 3DS action. */
  private[spi] def requiresAction(subscription: Subscription)(implicit
    requestOptions: RequestOptions
  ): Boolean = {
    Try {
      Option(subscription.getLatestInvoice).exists { invoiceId =>
        val invoice = Invoice.retrieve(invoiceId, requestOptions)
        Option(invoice.getPaymentIntent).exists { piId =>
          val pi = PaymentIntent.retrieve(piId, requestOptions)
          pi.getStatus == "requires_action"
        }
      }
    }.getOrElse(false)
  }

  /** @param userId
    *   - Provider user id
    * @param walletId
    *   - Provider wallet id
    * @param cardId
    *   - Provider card id
    * @param recurringPayment
    *   - recurring payment to register
    * @return
    *   recurring card payment registration result
    */
  override def registerRecurringCardPayment(
    userId: String,
    walletId: String,
    cardId: String,
    recurringPayment: RecurringPayment
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    if (!recurringPayment.`type`.isCard) {
      mlog.warn(
        s"Only card recurring payments are supported by Stripe, got: ${recurringPayment.`type`}"
      )
      return None
    }

    Try {
      implicit val requestOptions: RequestOptions = StripeApi().requestOptions()
      val (interval, intervalCount) = recurringPayment.frequency
        .map(toStripeInterval)
        .getOrElse(("month", 1L))

      val metadata = recurringPayment.metadata

      // Build price_data — product from per-subscription metadata
      val priceDataBuilder = SubscriptionCreateParams.Item.PriceData
        .builder()
        .setCurrency(recurringPayment.currency.toLowerCase)
        .setUnitAmount(recurringPayment.firstDebitedAmount.toLong)
        .setRecurring(
          SubscriptionCreateParams.Item.PriceData.Recurring
            .builder()
            .setInterval(
              SubscriptionCreateParams.Item.PriceData.Recurring.Interval.valueOf(
                interval.toUpperCase
              )
            )
            .setIntervalCount(intervalCount)
            .build()
        )

      val productId = metadata.get("stripe_product_id").getOrElse {
        // Create an inline product when no pre-existing product ID is provided
        val product = Product.create(
          ProductCreateParams
            .builder()
            .setName(metadata.getOrElse("product_name", "Recurring Payment"))
            .build(),
          requestOptions
        )
        product.getId
      }
      priceDataBuilder.setProduct(productId)

      val params = SubscriptionCreateParams
        .builder()
        .setCustomer(userId)
        .setDefaultPaymentMethod(cardId)
        .addItem(
          SubscriptionCreateParams.Item
            .builder()
            .setPriceData(priceDataBuilder.build())
            .build()
        )
        .putMetadata("transaction_type", "recurring_payment")
        .putMetadata("recurring_payment_type", "card")

      recurringPayment.externalReference.foreach(ref =>
        params.putMetadata("external_reference", ref)
      )
      recurringPayment.statementDescriptor.foreach(sd => params.setDescription(sd))

      // Route payment to connected account if applicable
      // walletId is a Stripe connected account only if it starts with "acct_"
      if (walletId.startsWith("acct_") && walletId != userId) {
        params.setTransferData(
          SubscriptionCreateParams.TransferData
            .builder()
            .setDestination(walletId)
            .build()
        )
        if (recurringPayment.firstDebitedAmount > 0) {
          params.setApplicationFeePercent(
            java.math.BigDecimal.valueOf(
              recurringPayment.firstFeesAmount.toDouble /
              recurringPayment.firstDebitedAmount * 100
            )
          )
        }
      }

      // Future start date: use trial_end to defer first billing
      recurringPayment.startDate.foreach { startDate =>
        val startEpoch = startDate.toInstant.getEpochSecond
        if (startEpoch > java.time.Instant.now().getEpochSecond) {
          params.setTrialEnd(startEpoch)
        }
      }

      // End date: schedule automatic cancellation
      recurringPayment.endDate.foreach { endDate =>
        params.setCancelAt(endDate.toInstant.getEpochSecond)
      }

      // Idempotency key to prevent duplicate subscriptions on retry
      val idempotencyKey = s"$userId-${recurringPayment.createdDate.getTime}"
      val options = requestOptions
        .toBuilderFullCopy()
        .setIdempotencyKey(idempotencyKey)
        .build()

      mlog.info(s"Creating Stripe subscription for customer: $userId")
      val subscription = Subscription.create(params.build(), options)

      val isFutureStart = recurringPayment.startDate.exists { sd =>
        sd.toInstant.getEpochSecond > java.time.Instant.now().getEpochSecond
      }
      val status = if (isFutureStart) {
        RecurringPayment.RecurringCardPaymentStatus.CREATED
      } else {
        toRecurringCardPaymentStatus(
          subscription.getStatus,
          requiresAction(subscription)
        )
      }

      RecurringPayment.RecurringCardPaymentResult.defaultInstance
        .withId(subscription.getId)
        .withStatus(status)
    } match {
      case Success(result) =>
        mlog.info(s"Stripe subscription created: ${result.id} -> ${result.status}")
        Some(result)
      case Failure(f) =>
        mlog.error(s"Failed to register recurring card payment: ${f.getMessage}", f)
        None
    }
  }

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @return
    *   recurring card payment registration result
    */
  override def loadRecurringCardPayment(
    recurringPayInRegistrationId: String
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    Try {
      implicit val requestOptions: RequestOptions = StripeApi().requestOptions()
      val subscription = Subscription.retrieve(recurringPayInRegistrationId, requestOptions)
      val status = toRecurringCardPaymentStatus(
        subscription.getStatus,
        requiresAction(subscription)
      )
      RecurringPayment.RecurringCardPaymentResult.defaultInstance
        .withId(subscription.getId)
        .withStatus(status)
    } match {
      case Success(result) => Some(result)
      case Failure(f) =>
        mlog.error(s"Failed to load recurring card payment: ${f.getMessage}", f)
        None
    }
  }

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @param cardId
    *   - Provider card id
    * @param status
    *   - optional recurring payment status
    * @return
    *   recurring card payment registration updated result
    */
  override def updateRecurringCardPaymentRegistration(
    recurringPayInRegistrationId: String,
    cardId: Option[String],
    status: Option[RecurringPayment.RecurringCardPaymentStatus]
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    Try {
      implicit val requestOptions: RequestOptions = StripeApi().requestOptions()
      val subscription = Subscription.retrieve(recurringPayInRegistrationId, requestOptions)

      status match {
        case Some(s) if s.isEnded =>
          mlog.info(s"Canceling subscription: $recurringPayInRegistrationId")
          val canceled =
            subscription.cancel(SubscriptionCancelParams.builder().build(), requestOptions)
          RecurringPayment.RecurringCardPaymentResult.defaultInstance
            .withId(canceled.getId)
            .withStatus(RecurringPayment.RecurringCardPaymentStatus.ENDED)
        case _ =>
          val params = SubscriptionUpdateParams.builder()
          cardId.foreach(id => params.setDefaultPaymentMethod(id))
          val updated = subscription.update(params.build(), requestOptions)
          RecurringPayment.RecurringCardPaymentResult.defaultInstance
            .withId(updated.getId)
            .withStatus(
              toRecurringCardPaymentStatus(
                updated.getStatus,
                requiresAction(updated)
              )
            )
      }
    } match {
      case Success(result) =>
        mlog.info(s"Updated recurring card payment: ${result.id} -> ${result.status}")
        Some(result)
      case Failure(f) =>
        mlog.error(s"Failed to update recurring card payment: ${f.getMessage}", f)
        None
    }
  }

  /** @param recurringPaymentTransaction
    *   - recurring payment transaction
    * @return
    *   resulted payIn transaction
    */
  override def createRecurringCardPayment(
    recurringPaymentTransaction: RecurringPaymentTransaction
  ): Option[Transaction] = {
    Try {
      val requestOptions = StripeApi().requestOptions()
      val subscriptionId = recurringPaymentTransaction.recurringPaymentRegistrationId

      // Retrieve the latest invoice for this subscription
      // Stripe returns invoices in reverse chronological order by default.
      // The code handles all statuses: draft → finalize+pay, open → pay,
      // paid/void/uncollectible → return as-is (idempotent).
      val invoiceListParams = InvoiceListParams
        .builder()
        .setSubscription(subscriptionId)
        .setLimit(1L)
        .build()
      val invoices = Invoice.list(invoiceListParams, requestOptions)

      invoices.getData.asScala.headOption match {
        case Some(invoice) =>
          // Handle invoice based on status — guard against double-pay
          val finalInvoice = invoice.getStatus match {
            case "draft" =>
              val finalized = invoice.finalizeInvoice(requestOptions)
              finalized.pay(requestOptions)
            case "open" =>
              invoice.pay(requestOptions)
            case _ =>
              // "paid", "void", "uncollectible" — return as-is (idempotent)
              invoice
          }

          val paymentIntentId = Option(finalInvoice.getPaymentIntent)
          val status = finalInvoice.getStatus match {
            case "paid"          => Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
            case "void"          => Transaction.TransactionStatus.TRANSACTION_FAILED
            case "uncollectible" => Transaction.TransactionStatus.TRANSACTION_FAILED
            case _               => Transaction.TransactionStatus.TRANSACTION_CREATED
          }

          Transaction()
            .withId(paymentIntentId.getOrElse(finalInvoice.getId))
            .withOrderUuid(recurringPaymentTransaction.externalUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.PAYIN)
            .withPaymentType(Transaction.PaymentType.CARD)
            .withAmount(finalInvoice.getAmountPaid.intValue())
            .withFees(
              Option(finalInvoice.getApplicationFeeAmount)
                .map(_.intValue())
                .getOrElse(0)
            )
            .withCurrency(finalInvoice.getCurrency)
            .withResultCode(finalInvoice.getStatus)
            .withStatus(status)
            .withAuthorId(finalInvoice.getCustomer)
            .copy(
              recurringPayInRegistrationId = Some(subscriptionId)
            )

        case None =>
          throw new RuntimeException(
            s"No invoice found for subscription: $subscriptionId"
          )
      }
    } match {
      case Success(transaction) =>
        mlog.info(
          s"Recurring card payment for subscription " +
          s"${recurringPaymentTransaction.recurringPaymentRegistrationId} -> " +
          s"${transaction.id} (${transaction.status})"
        )
        Some(transaction)
      case Failure(f) =>
        mlog.error(s"Failed to create recurring card payment: ${f.getMessage}", f)
        None
    }
  }
}
