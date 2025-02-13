package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{RefundTransaction, Transaction}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{
  ApplicationFee,
  Charge,
  PaymentIntent,
  Payout,
  Refund,
  Transfer,
  TransferReversal
}
import com.stripe.param.{
  FeeRefundCollectionCreateParams,
  PayoutCancelParams,
  RefundCreateParams,
  TransferReversalCollectionCreateParams
}

import scala.util.{Failure, Success, Try}
import collection.JavaConverters._

trait StripeRefundApi extends RefundApi { _: StripeContext =>

  /** @param maybeRefundTransaction
    *   - refund transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   refund transaction result
    */
  override def refund(
    maybeRefundTransaction: Option[RefundTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    maybeRefundTransaction match {
      case Some(refundTransaction) =>
        mlog.info(
          s"Processing refund transaction for order: ${refundTransaction.orderUuid} -> ${asJson(refundTransaction)}"
        )

        Try {
          val requestOptions = StripeApi().requestOptions()

          val transactionId = refundTransaction.payInTransactionId // TODO rename this field

          (if (transactionId.startsWith("tr_")) { // transfer

             val transfer = Transfer.retrieve(transactionId, requestOptions)

             val refundApplicationFee =
               Option(transfer.getSourceTransaction) match {
                 case Some(sourceTransaction)
                     if sourceTransaction
                       .startsWith("pi_") || sourceTransaction.startsWith("ch_") =>
                   false
                 case _ => true
               }

             val params =
               TransferReversalCollectionCreateParams
                 .builder()
                 .setAmount(Math.min(transfer.getAmount.intValue(), refundTransaction.refundAmount))
                 .setRefundApplicationFee(refundApplicationFee)
                 .putMetadata("order_uuid", refundTransaction.orderUuid)
                 .putMetadata("author_id", refundTransaction.authorId)
                 .putMetadata("reason_message", refundTransaction.reasonMessage)

             mlog.info(
               s"Processing transfer reversal for order: ${refundTransaction.orderUuid} -> ${new Gson()
                 .toJson(params)}"
             )

             val transferReversal = transfer.getReversals.create(params.build(), requestOptions)

             Option(transfer.getSourceTransaction) match {
               case Some(sourceTransaction) =>
                 if (sourceTransaction.startsWith("pi_"))
                   PaymentIntent.retrieve(sourceTransaction, requestOptions)
                 else if (sourceTransaction.startsWith("ch_"))
                   Charge.retrieve(sourceTransaction, requestOptions)
                 else
                   transferReversal
               case _ =>
                 transferReversal
             }
           } else if (transactionId.startsWith("po_")) { // pay out
             Payout.retrieve(transactionId, requestOptions)
           } else { // payment intent
             PaymentIntent.retrieve(transactionId, requestOptions)
           }) match {
            case payout: Payout =>
              payout.getStatus match {
                case "pending" =>
                  mlog.info(
                    s"Processing pay out cancellation for order: ${refundTransaction.orderUuid} -> ${new Gson()
                      .toJson(payout)}"
                  )
                  payout.cancel(PayoutCancelParams.builder().build(), requestOptions)
                case _ => payout
              }

            case payment: PaymentIntent => // refund the payment and reverse the transfer if any
              val transferReversal =
                Option(payment.getTransferData).flatMap(td => Option(td.getDestination)).isDefined

              val refundFees = transferReversal && payment.getApplicationFeeAmount > 0

              val refundApplicationFee =
                refundFees && refundTransaction.feesRefundAmount.getOrElse(0) == 0

              refundTransaction.feesRefundAmount match {
                case Some(feesRefundAmount)
                    if refundFees && feesRefundAmount > 0 => // refund application fees
                  // first retrieve the latest charge associated with the payment
                  val latestCharge = Charge.retrieve(payment.getLatestCharge, requestOptions)

                  // then retrieve the optional application fees associated with the charge
                  Option(latestCharge.getApplicationFee) match {

                    case Some(applicationFeeId) =>
                      val applicationFee = ApplicationFee.retrieve(applicationFeeId, requestOptions)

                      val params =
                        FeeRefundCollectionCreateParams
                          .builder()
                          .setAmount(
                            Math.min(payment.getApplicationFeeAmount.intValue(), feesRefundAmount)
                          )
                          .putMetadata("order_uuid", refundTransaction.orderUuid)
                          .putMetadata("author_id", refundTransaction.authorId)
                          .putMetadata("reason_message", refundTransaction.reasonMessage)

                      mlog.info(
                        s"Processing refund fees for order: ${refundTransaction.orderUuid} -> ${new Gson()
                          .toJson(params)}"
                      )

                      applicationFee.getRefunds.create(params.build(), requestOptions)

                    case _ =>

                  }

                case _ =>
              }

              val params =
                RefundCreateParams
                  .builder()
                  .setAmount(
                    Math.min(payment.getAmount.intValue(), refundTransaction.refundAmount)
                  )
                  .setCharge(payment.getLatestCharge)
                  .setReverseTransfer(transferReversal)
                  .setRefundApplicationFee(refundApplicationFee)
                  .putMetadata("order_uuid", refundTransaction.orderUuid)
                  .putMetadata("author_id", refundTransaction.authorId)
                  .putMetadata("reason_message", refundTransaction.reasonMessage)

              mlog.info(
                s"Processing refund payment for order: ${refundTransaction.orderUuid} -> ${new Gson()
                  .toJson(params)}"
              )

              if (refundTransaction.initializedByClient) {
                params.setReason(RefundCreateParams.Reason.REQUESTED_BY_CUSTOMER)
              }

              Refund.create(params.build(), requestOptions)

            case charge: Charge =>
              refundTransaction.feesRefundAmount match {
                case Some(feesRefundAmount)
                    if charge.getApplicationFeeAmount > 0 && feesRefundAmount > 0 => // refund application fees

                  // retrieve the optional application fees associated with the charge
                  Option(charge.getApplicationFee) match {

                    case Some(applicationFeeId) =>
                      val applicationFee = ApplicationFee.retrieve(applicationFeeId, requestOptions)

                      val params =
                        FeeRefundCollectionCreateParams
                          .builder()
                          .setAmount(
                            Math.min(charge.getApplicationFeeAmount.intValue(), feesRefundAmount)
                          )
                          .putMetadata("order_uuid", refundTransaction.orderUuid)
                          .putMetadata("author_id", refundTransaction.authorId)
                          .putMetadata("reason_message", refundTransaction.reasonMessage)

                      mlog.info(
                        s"Processing refund fees for order: ${refundTransaction.orderUuid} -> ${new Gson()
                          .toJson(params)}"
                      )

                      applicationFee.getRefunds.create(params.build(), requestOptions)

                    case _ =>

                  }

                case _ =>
              }

              val params =
                RefundCreateParams
                  .builder()
                  .setAmount(Math.min(charge.getAmount.intValue(), refundTransaction.refundAmount))
                  .setCharge(charge.getId)
                  .setReverseTransfer(false)
                  .setRefundApplicationFee(false)
                  .putMetadata("order_uuid", refundTransaction.orderUuid)
                  .putMetadata("author_id", refundTransaction.authorId)
                  .putMetadata("reason_message", refundTransaction.reasonMessage)

              mlog.info(
                s"Processing refund payment for order: ${refundTransaction.orderUuid} -> ${new Gson()
                  .toJson(params)}"
              )

              if (refundTransaction.initializedByClient) {
                params.setReason(RefundCreateParams.Reason.REQUESTED_BY_CUSTOMER)
              }

              Refund.create(params.build(), requestOptions)
            case transferReversal: TransferReversal => transferReversal
          }

        } match {
          case Success(payout: Payout) =>
            var transaction =
              Transaction()
                .withId(payout.getId)
                .withOrderUuid(refundTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REFUND)
                .withType(Transaction.TransactionType.PAYOUT)
                .withAmount(payout.getAmount.intValue())
                .withFees(Option(payout.getApplicationFeeAmount).map(_.toInt).getOrElse(0))
                .withCurrency(payout.getCurrency)
                .withReasonMessage(refundTransaction.reasonMessage)
                .withAuthorId(refundTransaction.authorId)
                .withSourceTransactionId(payout.getId)

            payout.getStatus match {
              case "canceled" =>
                transaction =
                  transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
                mlog.info(
                  s"Refund transaction for order: ${refundTransaction.orderUuid} processed successfully -> ${asJson(payout)}"
                )
              case _ =>
                transaction =
                  transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_FAILED)
                mlog.info(
                  s"Refund transaction for order: ${refundTransaction.orderUuid} failed -> ${asJson(payout)}"
                )

            }

            Some(transaction)

          case Success(transferReversal: TransferReversal) =>
            val transaction =
              Transaction()
                .withId(transferReversal.getId)
                .withOrderUuid(refundTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REFUND)
                .withType(Transaction.TransactionType.TRANSFER)
                .withAmount(transferReversal.getAmount.intValue())
                .withFees(0)
                .withCurrency(transferReversal.getCurrency)
                .withReasonMessage(refundTransaction.reasonMessage)
                .withAuthorId(refundTransaction.authorId)
                .withSourceTransactionId(transferReversal.getTransfer)

            mlog.info(
              s"Refund transaction for order: ${refundTransaction.orderUuid} processed successfully -> ${asJson(transferReversal)}"
            )

            Some(transaction)

          case Success(refund: Refund) =>
            val status = refund.getStatus

            var transaction =
              Transaction()
                .withId(refund.getId)
                .withOrderUuid(refundTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REFUND)
                .withType(Transaction.TransactionType.PAYIN)
                .withAmount(refund.getAmount.intValue())
                .withFees(0)
                .withCurrency(refund.getCurrency)
                .withResultCode(status)
                .withReasonMessage(refundTransaction.reasonMessage)
                .withAuthorId(refundTransaction.authorId)
                .withSourceTransactionId(
                  Option(refund.getPaymentIntent).getOrElse(refund.getCharge)
                )

            Option(refund.getDestinationDetails) match {
              case Some(destinationDetails) =>
                destinationDetails.getType match {
                  case "card" =>
                    transaction = transaction.withPaymentType(Transaction.PaymentType.CARD)
                  case "bank_account" =>
                    transaction = transaction.withPaymentType(Transaction.PaymentType.BANK_WIRE)
                  case "paypal" =>
                    transaction = transaction.withPaymentType(Transaction.PaymentType.PAYPAL)
                  case _ =>
                }
              case _ =>
            }

            status match {
              case "succeeded" =>
                transaction =
                  transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
              case "failed" =>
                transaction = transaction
                  .withStatus(Transaction.TransactionStatus.TRANSACTION_FAILED)
                  .copy(
                    resultMessage = refund.getFailureReason
                  )
              case "canceled" =>
                transaction =
                  transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_CANCELED)
              case _ =>
                transaction =
                  transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_CREATED)
            }

            mlog.info(
              s"Refund transaction for order: ${refundTransaction.orderUuid} processed successfully -> ${asJson(refund)}"
            )

            Some(transaction)

          case Failure(f) =>
            mlog.error(
              s"Error processing refund transaction for order: ${refundTransaction.orderUuid}",
              f
            )
            None
        }
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   Refund transaction
    */
  override def loadRefundTransaction(
    orderUuid: String,
    transactionId: String
  ): Option[Transaction] = {
    Try {
      Refund.retrieve(transactionId, StripeApi().requestOptions())
    } match {
      case Success(refund: Refund) =>
        val status = refund.getStatus

        val metadata = refund.getMetadata.asScala

        val reasonMessage = metadata.getOrElse("reason_message", "")

        val authorId = metadata.getOrElse("author_id", "")

        var transaction = // TODO payment type
          Transaction()
            .withId(refund.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REFUND)
            .withType(Transaction.TransactionType.PAYIN)
            .withAmount(refund.getAmount.intValue())
            .withFees(0)
            .withCurrency(refund.getCurrency)
            .withResultCode(status)
            .withReasonMessage(reasonMessage)
            .withAuthorId(authorId)
            .withSourceTransactionId(refund.getPaymentIntent)

        Option(refund.getDestinationDetails) match {
          case Some(destinationDetails) =>
            destinationDetails.getType match {
              case "card" =>
                transaction = transaction.withPaymentType(Transaction.PaymentType.CARD)
              case "bank_account" =>
                transaction = transaction.withPaymentType(Transaction.PaymentType.BANK_WIRE)
              case "paypal" =>
                transaction = transaction.withPaymentType(Transaction.PaymentType.PAYPAL)
              case _ =>
            }
          case _ =>
        }

        status match {
          case "succeeded" =>
            transaction =
              transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
          case "failed" =>
            transaction = transaction
              .withStatus(Transaction.TransactionStatus.TRANSACTION_FAILED)
              .copy(
                resultMessage = refund.getFailureReason
              )
          case "canceled" =>
            transaction = transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_CANCELED)
          case _ =>
            transaction = transaction.withStatus(Transaction.TransactionStatus.TRANSACTION_CREATED)
        }

        mlog.info(
          s"Refund transaction for order: $orderUuid loaded successfully -> ${asJson(refund)}"
        )

        Some(transaction)

      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }
}
