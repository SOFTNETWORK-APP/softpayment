package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{PayOutTransaction, Transaction, TransferTransaction}
import app.softnetwork.serialization.asJson
import com.google.gson.Gson
import com.stripe.model.{PaymentIntent, Payout, Transfer}
import com.stripe.param.PayoutCreateParams

import scala.util.{Failure, Success, Try}
import collection.JavaConverters._

trait StripePayOutApi extends PayOutApi {
  _: StripeContext with StripeTransferApi with StripeBalanceApi =>

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

        val debitedAmount = payOutTransaction.debitedAmount

        val feesAmount = payOutTransaction.feesAmount

        Try {
          payOutTransaction.payInTransactionId match {
            case Some(payInTransactionId) if payInTransactionId.startsWith("pi_") =>
              // transfer funds from this payment intent to the connected stripe account
              val transferTransaction =
                TransferTransaction.defaultInstance
                  .withOrderUuid(payOutTransaction.orderUuid)
                  .withPayInTransactionId(payInTransactionId)
                  .withDebitedAmount(debitedAmount)
                  .withFeesAmount(feesAmount)
                  .withCurrency(payOutTransaction.currency)
                  .withAuthorId(payOutTransaction.authorId)
                  .withCreditedUserId(
                    payOutTransaction.creditedUserId
                  ) // the connected stripe account
                  .withDebitedWalletId(payOutTransaction.debitedWalletId)
                  .copy(
                    externalReference = payOutTransaction.externalReference,
                    statementDescriptor = payOutTransaction.statementDescriptor
                  )

              transfer(Some(transferTransaction))

            case None =>
              // either receive funds from Stripe or
              // send funds to the bank account of the connected Stripe account

              var stripeAccount: Option[String] = None

              val params =
                PayoutCreateParams
                  .builder()
                  .setCurrency(payOutTransaction.currency)
                  .setMethod(PayoutCreateParams.Method.STANDARD)
//                  .setSourceType(PayoutCreateParams.SourceType.CARD)
                  .putMetadata("order_uuid", payOutTransaction.orderUuid.take(500))
                  .putMetadata(
                    "external_reference",
                    payOutTransaction.externalReference.getOrElse("")
                  )
                  .putMetadata("debited_amount", payOutTransaction.debitedAmount.toString)
                  .putMetadata("fees_amount", payOutTransaction.feesAmount.toString)

              val amountToTransfer = debitedAmount - feesAmount

              var availableAmount = 0

              if (payOutTransaction.bankAccountId.trim.nonEmpty) {
                // we send funds to the specified bank account of a connected Stripe account

                stripeAccount = Option(payOutTransaction.creditedUserId)

                // load balance
                availableAmount =
                  loadBalance(payOutTransaction.currency, Option(payOutTransaction.creditedUserId))
                    .getOrElse(0)

                mlog.info(
                  s"balance available amount for ${payOutTransaction.creditedUserId} is $availableAmount"
                )

                mlog.info(
                  s"amount to transfer to ${payOutTransaction.bankAccountId} is $amountToTransfer"
                )

                params
                  .setAmount(amountToTransfer)
                  .setDestination(payOutTransaction.bankAccountId)
                  .putMetadata("available_amount", s"$availableAmount")
              } else {
                // we receive funds from Stripe
                // load balance
                availableAmount = loadBalance(payOutTransaction.currency, None).getOrElse(0)

                mlog.info(s"balance available amount for our stripe account is $availableAmount")

                mlog.info(s"amount to transfer to our stripe account is $amountToTransfer")

                params
                  .setAmount(amountToTransfer)
                  .putMetadata("available_amount", s"$availableAmount")
              }

              payOutTransaction.statementDescriptor match {
                case Some(statementDescriptor) =>
                  params.setStatementDescriptor(statementDescriptor)
                case _ =>
              }

              payOutTransaction.externalReference match {
                case Some(externalReference) =>
                  params.putMetadata("external_reference", externalReference)
                case _ =>
              }

              if (amountToTransfer <= availableAmount) {
                val payout = params.build()
                mlog.info(s"payout: ${new Gson().toJson(payout)}")
                Payout.create(payout, StripeApi().requestOptions(stripeAccount))
              } else {
                throw new Exception("Insufficient funds")
              }
          }

        } match {
          case Success(transferTransaction: Option[Transaction]) =>
            transferTransaction

          case Success(payOut: Payout) =>
            val status = payOut.getStatus

            val creditedUserId = Option(payOutTransaction.creditedUserId)

            var transaction =
              Transaction()
                .withId(payOut.getId)
                .withOrderUuid(payOutTransaction.orderUuid)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.PAYOUT)
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
                .withAmount(debitedAmount)
                .withFees(feesAmount)
                .withCurrency(payOut.getCurrency)
                .withAuthorId(payOutTransaction.authorId)
                .withDebitedWalletId(payOutTransaction.debitedWalletId)
                .withTransferAmount(
                  payOut.getAmount.intValue()
                ) // should be equal to debitedAmount - feesAmount
                .copy(
                  creditedUserId = creditedUserId,
                  sourceTransactionId = payOutTransaction.payInTransactionId,
                  externalReference = payOutTransaction.externalReference
                )

            status match {
              case "paid" =>
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
                )
              case "pending" | "in_transit" => // TODO add a webhook to handle this case
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_CREATED
                )
              case "canceled" =>
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_CANCELED
                )
              case _ =>
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_FAILED
                )
            }

            mlog.info(
              s"Pay out executed for order ${transaction.orderUuid} -> ${asJson(transaction)}"
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
      val requestOptions = StripeApi().requestOptions()
      if (transactionId.startsWith("tr_")) {
        Transfer.retrieve(transactionId, requestOptions)
      } else if (transactionId.startsWith("po_")) {
        Payout.retrieve(transactionId, requestOptions)
      } else {
        PaymentIntent.retrieve(transactionId, requestOptions)
      }
    } match {
      case Success(payOut: Payout) =>
        val status = payOut.getStatus

        val metadata = payOut.getMetadata.asScala

        val feesAmount = metadata.get("fees_amount").map(_.toInt).getOrElse(0)

        var transaction =
          Transaction()
            .withId(payOut.getId)
            .withOrderUuid(orderUuid)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.TRANSFER)
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            .withAmount(payOut.getAmount.intValue() + feesAmount)
            .withFees(feesAmount)
            .withCurrency(payOut.getCurrency)
            .withCreditedUserId(payOut.getDestination)
            .withTransferAmount(payOut.getAmount.intValue())

        status match {
          case "paid" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
            )
          case "pending" | "in_transit" => // TODO add a webhook to handle this case
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_CREATED
            )
          case "canceled" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_CANCELED
            )
          case _ =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_FAILED
            )
        }

        mlog.info(
          s"Pay out transaction retrieved for order ${transaction.orderUuid} -> ${asJson(transaction)}"
        )

        Some(transaction)

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
            .withStatus(Transaction.TransactionStatus.TRANSACTION_SUCCEEDED) //FIXME
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

        status match {
          case "succeeded" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
            )
          case "processing" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
          case "canceled" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CANCELED)
          case _ => // requires_action, requires_capture, requires_confirmation, requires_payment_method
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_FAILED)
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
