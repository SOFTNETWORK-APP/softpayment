package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{Transaction, TransferTransaction}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{Balance, PaymentIntent, Transfer}
import com.stripe.param.{
  PaymentIntentCaptureParams,
  PaymentIntentUpdateParams,
  TransferCreateParams
}

import collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait StripeTransferApi extends TransferApi { _: StripeContext =>

  /** @param maybeTransferTransaction
    *   - transfer transaction
    * @return
    *   transfer transaction result
    */
  override def transfer(
    maybeTransferTransaction: Option[TransferTransaction]
  ): Option[Transaction] = {
    maybeTransferTransaction match {
      case Some(transferTransaction) =>
        mlog.info(
          s"Processing transfer transaction for order: ${transferTransaction.orderUuid} -> ${asJson(transferTransaction)}"
        )

        var debitedAmount = transferTransaction.debitedAmount

        var feesAmount = transferTransaction.feesAmount

        Try {
          val requestOptions = StripeApi().requestOptions

          transferTransaction.payInTransactionId match {
            // case of a payment intent
            case Some(transactionId) if transactionId.startsWith("pi_") =>
              var resource =
                PaymentIntent.retrieve(transactionId, requestOptions)

              // optionally update fees amount if different from the one in the payment intent and
              // the payment intent has not been captured yet
              resource =
                Option(resource.getTransferData).flatMap(td => Option(td.getDestination)) match {
                  case Some(_)
                      if feesAmount != Option(resource.getApplicationFeeAmount)
                        .map(_.intValue())
                        .getOrElse(0) && resource.getStatus == "requires_capture" =>
                    resource.update(
                      PaymentIntentUpdateParams
                        .builder()
                        .setApplicationFeeAmount(feesAmount)
                        .build(),
                      requestOptions
                    )
                  case _ => resource
                }

              val payment =
                resource.getStatus match {
                  case "requires_capture" => // we capture the funds
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

                case Some(destination) if destination != transferTransaction.creditedUserId =>
                  throw new Exception(
                    s"Destination account does not match the credited user id: $destination != ${transferTransaction.creditedUserId}"
                  )

                case Some(_) => // no need for transfer
                  debitedAmount = payment.getAmountReceived.intValue()
                  feesAmount =
                    Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0)
                  payment

                case None =>
                  debitedAmount = payment.getAmountReceived.intValue()

                  val amountToTransfer = debitedAmount - feesAmount

                  val params =
                    TransferCreateParams
                      .builder()
                      .setAmount(amountToTransfer)
                      .setCurrency(transferTransaction.currency)
                      .setDestination(transferTransaction.creditedUserId)
                      .setSourceTransaction(payment.getLatestCharge)
                      .putMetadata("debited_amount", debitedAmount.toString)
                      .putMetadata("fees_amount", feesAmount.toString)
                      .putMetadata("amount_to_transfer", amountToTransfer.toString)

                  transferTransaction.orderUuid match {
                    case Some(orderUuid) =>
                      params.setTransferGroup(orderUuid)
                      params.putMetadata("order_uuid", orderUuid)
                    case _ =>
                      Option(payment.getTransferGroup).foreach { transferGroup =>
                        params.setTransferGroup(transferGroup)
                        params.putMetadata("order_uuid", transferGroup)
                      }
                  }

                  transferTransaction.externalReference match {
                    case Some(externalReference) =>
                      params.putMetadata("external_reference", externalReference)
                    case _ =>
                  }

                  mlog.info(
                    s"Creating transfer for order ${transferTransaction.orderUuid} -> ${new Gson()
                      .toJson(params.build())}"
                  )

                  Transfer.create(params.build(), requestOptions)
              }

            case _ =>
              val amountToTransfer = debitedAmount - feesAmount

              val availableAmount =
                Balance
                  .retrieve(requestOptions)
                  .getAvailable
                  .asScala
                  .find(_.getCurrency == transferTransaction.currency) match {
                  case Some(balance) =>
                    balance.getAmount.intValue()
                  case None =>
                    0
                }

              val params =
                TransferCreateParams
                  .builder()
                  .setAmount(Math.min(amountToTransfer, availableAmount))
                  .setDestination(transferTransaction.creditedUserId)
                  .setCurrency(transferTransaction.currency)
                  .putMetadata("available_amount", availableAmount.toString)
                  .putMetadata("debited_amount", transferTransaction.debitedAmount.toString)
                  .putMetadata("fees_amount", transferTransaction.feesAmount.toString)
                  .putMetadata("amount_to_transfer", amountToTransfer.toString)

              transferTransaction.orderUuid match {
                case Some(orderUuid) =>
                  params.setTransferGroup(orderUuid)
                  params.putMetadata("order_uuid", orderUuid)
                case _ =>
              }

              Transfer.create(params.build(), requestOptions)

          }

        } match {
          case Success(payment: PaymentIntent) =>
            val status = payment.getStatus

            val creditedUserId = Option(transferTransaction.creditedUserId)
            // Option(payment.getTransferData).map(_.getDestination)

            var transaction =
              Transaction()
                .withId(payment.getId)
                .withOrderUuid(
                  Option(payment.getTransferGroup)
                    .orElse(transferTransaction.orderUuid)
                    .getOrElse("")
                )
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.PAYOUT)
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
                .withAmount(debitedAmount)
                .withFees(feesAmount)
                .withCurrency(payment.getCurrency)
                .withAuthorId(
                  Option(payment.getCustomer).getOrElse(transferTransaction.authorId)
                )
                .withDebitedWalletId(transferTransaction.debitedWalletId)
                .copy(
                  creditedUserId = creditedUserId,
                  sourceTransactionId = transferTransaction.payInTransactionId,
                  externalReference = transferTransaction.externalReference
                )

            status match {
              case "succeeded" =>
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                )
              case "processing" =>
                transaction =
                  transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
              case "cancelled" =>
                transaction =
                  transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CANCELED)
              case _ => //requires_action, requires_confirmation, requires_payment_method
                transaction =
                  transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
            }

            mlog.info(
              s"Transfer executed for order ${transaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)

          case Success(transfer: Transfer) =>
            val transaction =
              Transaction()
                .withId(transfer.getId)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.TRANSFER)
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
                .withAmount(transferTransaction.debitedAmount)
                .withFees(transferTransaction.feesAmount)
                .withTransferAmount(transfer.getAmount.intValue())
                .withCurrency(transfer.getCurrency)
                .withAuthorId(transferTransaction.authorId)
                .withCreditedUserId(transfer.getDestination)
                .withDebitedWalletId(transferTransaction.debitedWalletId)
                .withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
                .copy(
                  orderUuid = transferTransaction.orderUuid.getOrElse(""),
                  sourceTransactionId = transferTransaction.payInTransactionId,
                  externalReference = transferTransaction.externalReference
                )

            mlog.info(
              s"Transfer transaction created for order ${transaction.orderUuid} -> ${asJson(transaction)}"
            )

            Some(transaction)

          case Failure(f) =>
            mlog.error(
              s"Error processing transfer transaction for order: ${transferTransaction.orderUuid} -> ${asJson(transferTransaction)}",
              f
            )
            None
        }
      case _ => None
    }
  }

  /** @param transactionId
    *   - transaction id
    * @return
    *   transfer transaction
    */
  override def loadTransfer(transactionId: String): Option[Transaction] = {
    Try {
      if (transactionId.startsWith("pi_")) {
        PaymentIntent.retrieve(transactionId, StripeApi().requestOptions)
      } else {
        Transfer.retrieve(transactionId, StripeApi().requestOptions)
      }
    } match {
      case Success(payment: PaymentIntent) =>
        val status = payment.getStatus

        val creditedUserId = Option(payment.getTransferData).map(_.getDestination)
        val amountToTransfer = payment.getAmountReceived.intValue()
        val feesAmount = Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0)

        var transaction =
          Transaction()
            .withId(payment.getId)
            .withOrderUuid(Option(payment.getTransferGroup).getOrElse(""))
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.TRANSFER)
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            .withAmount(amountToTransfer + feesAmount)
            .withFees(feesAmount)
            .withCurrency(payment.getCurrency)
            .withAuthorId(Option(payment.getCustomer).getOrElse(""))
            .withCreditedUserId(creditedUserId.getOrElse(""))
            .withTransferAmount(amountToTransfer)

        status match {
          case "succeeded" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
            )
          case "processing" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
          case "cancelled" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CANCELED)
          case _ => //requires_action, requires_confirmation, requires_payment_method
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
        }

        mlog.info(
          s"Transfer transaction retrieved for $transactionId -> ${asJson(transaction)}"
        )

        Some(transaction)

      case Success(transfer: Transfer) =>
        val metadata = transfer.getMetadata.asScala
        val orderUuid =
          metadata.get("order_uuid").orElse(Option(transfer.getTransferGroup)).getOrElse("")
        val amountToTransfer = metadata.get("amount_to_transfer").map(_.toInt).getOrElse(0)
        val feesAmount = metadata.get("fees_amount").map(_.toInt).getOrElse(0)

        val transaction =
          Transaction()
            .withId(transfer.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.TRANSFER)
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            .withAmount(amountToTransfer + feesAmount)
            .withFees(feesAmount)
            .withCurrency(transfer.getCurrency)
            .withCreditedUserId(transfer.getDestination)
            .withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
            .withTransferAmount(transfer.getAmount.intValue())

        mlog.info(
          s"Transfer transaction retrieved for $transactionId -> ${asJson(transaction)}"
        )

        Some(transaction)

      case Failure(f) =>
        mlog.error(
          s"Error retrieving transfer transaction for $transactionId",
          f
        )
        None
    }
  }
}
