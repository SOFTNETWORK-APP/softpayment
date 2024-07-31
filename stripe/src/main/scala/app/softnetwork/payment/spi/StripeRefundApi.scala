package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{RefundTransaction, Transaction}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{Charge, PaymentIntent, Refund, Transfer, TransferReversal}
import com.stripe.param.{RefundCreateParams, TransferReversalCollectionCreateParams}

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
          val requestOptions = StripeApi().requestOptions

          val transactionId = refundTransaction.payInTransactionId // TODO rename this field

          (if (transactionId.startsWith("tr_")) {

             val transfer = Transfer.retrieve(transactionId, requestOptions)

             val params =
               TransferReversalCollectionCreateParams
                 .builder()
                 .setAmount(Math.min(transfer.getAmount.intValue(), refundTransaction.refundAmount))
                 .setRefundApplicationFee(true)
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
           } else {
             PaymentIntent.retrieve(transactionId, requestOptions)
           }) match {
            case payment: PaymentIntent =>
              val params =
                RefundCreateParams
                  .builder()
                  .setAmount(
                    Math.min(payment.getAmountReceived.intValue(), refundTransaction.refundAmount)
                  )
                  .setCharge(payment.getLatestCharge)
                  .setReverseTransfer(false)
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
              val params =
                RefundCreateParams
                  .builder()
                  .setAmount(Math.min(charge.getAmount.intValue(), refundTransaction.refundAmount))
                  .setCharge(charge.getId)
                  .setReverseTransfer(false)
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
      Refund.retrieve(transactionId, StripeApi().requestOptions)
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
