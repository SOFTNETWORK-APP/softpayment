package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{Transaction, TransferTransaction}
import app.softnetwork.serialization.asJson
import com.stripe.model.{Balance, Transfer}
import com.stripe.param.TransferCreateParams

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
        Try {
          val requestOptions = StripeApi().requestOptions

          val amountToTransfer =
            transferTransaction.debitedAmount - transferTransaction.feesAmount

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

        } match {
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
                  sourceTransactionId = transferTransaction.payInTransactionId
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
      Transfer.retrieve(transactionId, StripeApi().requestOptions)
    } match {
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
          s"Pay out transaction retrieved for order ${transaction.orderUuid} -> ${asJson(transaction)}"
        )

        Some(transaction)
    }
  }
}
