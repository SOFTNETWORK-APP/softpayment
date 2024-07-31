package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{PayOutTransaction, Transaction}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{Charge, PaymentIntent, Transfer}
import com.stripe.param.{
  PaymentIntentCaptureParams,
  PaymentIntentUpdateParams,
  TransferCreateParams
}

import scala.util.{Failure, Success, Try}
import collection.JavaConverters._

trait StripePayOutApi extends PayOutApi { _: StripeContext =>

  /** @param maybePayOutTransaction
    *   - pay out transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay out transaction result
    */
  override def payOut(
    maybePayOutTransaction: Option[PayOutTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    maybePayOutTransaction match {
      case Some(payOutTransaction) =>
        mlog.info(
          s"Processing pay out transaction for order(s): ${payOutTransaction.orderUuid} -> ${asJson(payOutTransaction)}"
        )

        var debitedAmount = payOutTransaction.debitedAmount

        var feesAmount = payOutTransaction.feesAmount

        Try {
          val requestOptions = StripeApi().requestOptions

          var resource =
            PaymentIntent
              .retrieve(
                payOutTransaction.payInTransactionId.getOrElse(
                  throw new Exception("No pay in transaction id found")
                ),
                requestOptions
              )

          // optionally update fees amount
          resource =
            Option(resource.getTransferData).flatMap(td => Option(td.getDestination)) match {
              case Some(_)
                  if feesAmount != Option(resource.getApplicationFeeAmount)
                    .map(_.intValue())
                    .getOrElse(0) =>
                resource.update(
                  PaymentIntentUpdateParams.builder().setApplicationFeeAmount(feesAmount).build(),
                  requestOptions
                )
              case _ => resource
            }

          val payment =
            resource.getStatus match {
              case "requires_capture" => // should never happen
                val params =
                  PaymentIntentCaptureParams
                    .builder()
                    .setAmountToCapture(resource.getAmountCapturable)
                resource.capture(
                  params.build(),
                  requestOptions
                )
              case _ => resource
            }

          Option(payment.getTransferData).flatMap(td => Option(td.getDestination)) match {
            case Some(destination) if destination != payOutTransaction.creditedUserId =>
              throw new Exception(
                s"Destination account does not match the credited user id: $destination != ${payOutTransaction.creditedUserId}"
              )
            case Some(_) => // no need for transfer
              debitedAmount = payment.getAmountReceived.intValue()
              feesAmount = Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0)
              payment
            case None =>
              debitedAmount = payment.getAmountReceived.intValue()
              val amountToTransfer = debitedAmount - feesAmount
              val params =
                TransferCreateParams
                  .builder()
                  .setAmount(amountToTransfer)
                  .setCurrency(payOutTransaction.currency)
                  .setDestination(payOutTransaction.creditedUserId)
                  .setSourceTransaction(payment.getLatestCharge)
                  .setTransferGroup(
                    Option(payment.getTransferGroup).getOrElse(payOutTransaction.orderUuid)
                  )
                  .putMetadata("order_uuid", payOutTransaction.orderUuid)
                  .putMetadata("debited_amount", debitedAmount.toString)
                  .putMetadata("fees_amount", feesAmount.toString)
                  .putMetadata("amount_to_transfer", amountToTransfer.toString)
              payOutTransaction.externalReference match {
                case Some(externalReference) =>
                  params.putMetadata("external_reference", externalReference)
                case _ =>
              }

              mlog.info(s"Creating transfer for order ${payOutTransaction.orderUuid} -> ${new Gson()
                .toJson(params.build())}")

              Transfer.create(params.build(), requestOptions)
          }
        } match {
          case Success(payment: PaymentIntent) =>
            val status = payment.getStatus

            val creditedUserId = Option(payment.getTransferData).map(_.getDestination)

            var transaction =
              Transaction()
                .withId(payment.getId)
                .withOrderUuid(payOutTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.PAYOUT)
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
                .withAmount(debitedAmount)
                .withFees(feesAmount)
                .withCurrency(payment.getCurrency)
                .withAuthorId(
                  Option(payment.getCustomer).getOrElse(payOutTransaction.authorId)
                )
                .withDebitedWalletId(
                  Option(payment.getCustomer).getOrElse(payOutTransaction.debitedWalletId)
                )
                .copy(
                  creditedUserId = creditedUserId,
                  sourceTransactionId = payOutTransaction.payInTransactionId
                )

            if (status == "succeeded") {
              transaction = transaction.copy(
                status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
              )
            } else if (status == "processing") {
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
            } else { //canceled, requires_action, requires_capture, requires_confirmation, requires_payment_method
              transaction =
                transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
            }

            mlog.info(
              s"Pay out executed for order ${transaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)

          case Success(transfer: Transfer) =>
            val transaction =
              Transaction()
                .withId(transfer.getId)
                .withOrderUuid(payOutTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.TRANSFER)
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
                .withAmount(debitedAmount)
                .withFees(feesAmount)
                .withTransferAmount(
                  transfer.getAmount.intValue()
                ) // should be equal to debitedAmount - feesAmount
                .withCurrency(transfer.getCurrency)
                .withAuthorId(payOutTransaction.authorId)
                .withCreditedUserId(transfer.getDestination)
                .withDebitedWalletId(payOutTransaction.debitedWalletId)
                .withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
                .copy(
                  sourceTransactionId = payOutTransaction.payInTransactionId
                )

            mlog.info(
              s"Transfer transaction created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)

          case Failure(f) =>
            mlog.error(s"Error while processing pay out transaction: ${f.getMessage}", f)
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
    *   pay out transaction
    */
  override def loadPayOutTransaction(
    orderUuid: String,
    transactionId: String
  ): Option[Transaction] = {
    Try {
      val requestOptions = StripeApi().requestOptions
      if (transactionId.startsWith("tr_")) {
        Transfer.retrieve(transactionId, requestOptions)
      } else {
        PaymentIntent.retrieve(transactionId, requestOptions)
      }
    } match {
      case Success(transfer: Transfer) =>
        val metadata = transfer.getMetadata.asScala
        val feesAmount = metadata.get("fees_amount").map(_.toInt).getOrElse(0)

        val transaction =
          Transaction()
            .withId(transfer.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.TRANSFER)
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            .withAmount(transfer.getAmount.intValue() + feesAmount)
            .withFees(feesAmount)
            .withCurrency(transfer.getCurrency)
            .withCreditedUserId(transfer.getDestination)
            .withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
            .withTransferAmount(transfer.getAmount.intValue())

        mlog.info(
          s"Pay out transaction retrieved for order ${transaction.orderUuid} -> ${asJson(transaction)}"
        )

        Some(transaction)

      case Success(payment: PaymentIntent) =>
        val status = payment.getStatus

        val creditedUserId = Option(payment.getTransferData).map(_.getDestination)

        var transaction =
          Transaction()
            .withId(payment.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.PAYOUT)
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            .withAmount(payment.getAmount.intValue())
            .withFees(Option(payment.getApplicationFeeAmount.intValue()).getOrElse(0))
            .withCurrency(payment.getCurrency)
            .withAuthorId(payment.getCustomer)
            .withDebitedWalletId(payment.getCustomer)
            .copy(
              creditedUserId = creditedUserId
            )

        if (status == "succeeded") {
          transaction = transaction.copy(
            status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
          )
        } else if (status == "processing") {
          transaction = transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
        } else { //canceled, requires_action, requires_capture, requires_confirmation, requires_payment_method
          transaction = transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
        }

        mlog.info(
          s"Pay out transaction retrieved for order ${transaction.orderUuid} -> ${asJson(transaction)}"
        )

        Some(transaction)

      case Failure(f) =>
        mlog.error(s"Error while loading pay out transaction: ${f.getMessage}", f)
        None
    }
  }
}
