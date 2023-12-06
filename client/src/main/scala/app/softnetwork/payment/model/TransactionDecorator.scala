package app.softnetwork.payment.model

import java.time.Instant

trait TransactionDecorator { self: Transaction =>
  lazy val uuid: String = self.id

  lazy val view: TransactionView = TransactionView(self)
}

case class TransactionView(
  createdDate: Instant,
  lastUpdated: Instant,
  id: String,
  orderUuid: String,
  nature: Transaction.TransactionNature,
  `type`: Transaction.TransactionType,
  status: Transaction.TransactionStatus,
  amount: Int,
  currency: String,
  fees: Int,
//                           cardId: Option[String] = None,
  resultCode: String,
  resultMessage: String,
//                           redirectUrl: Option[String] = None,
  reasonMessage: Option[String] = None,
  authorId: String,
//                           creditedWalletId: Option[String] = None,
//                           creditedUserId: Option[String] = None,
//                           debitedWalletId: Option[String] = None,
//                           mandateId: Option[String] = None,
//                           preAuthorizationId: Option[String] = None,
  paymentType: Transaction.PaymentType
)

object TransactionView {
  def apply(transaction: Transaction): TransactionView = {
    import transaction._
    TransactionView(
      createdDate,
      lastUpdated,
      id,
      orderUuid,
      nature,
      `type`,
      status,
      amount,
      currency,
      fees,
//      cardId,
      resultCode,
      resultMessage,
//      redirectUrl,
      reasonMessage,
      authorId,
//      creditedWalletId,
//      creditedUserId,
//      debitedWalletId,
//      mandateId,
//      preAuthorizationId,
      paymentType
    )
  }
}
