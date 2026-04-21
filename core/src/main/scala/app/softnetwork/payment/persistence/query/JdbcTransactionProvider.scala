package app.softnetwork.payment.persistence.query

import app.softnetwork.payment.model.Transaction
import app.softnetwork.persistence.ManifestWrapper
import app.softnetwork.persistence.jdbc.query.ColumnMappedJdbcStateProvider

import slick.jdbc.{GetResult, JdbcProfile}

trait JdbcTransactionProvider
    extends ColumnMappedJdbcStateProvider[Transaction]
    with ManifestWrapper[Transaction] {
  _: JdbcProfile =>

  import api._

  override protected val manifestWrapper: ManifestW = ManifestW()

  protected implicit val getInstantResult: GetResult[java.time.Instant] =
    GetResult(r => r.nextTimestamp().toInstant)

  protected implicit val getOptInstantResult: GetResult[Option[java.time.Instant]] =
    GetResult(r => Option(r.nextTimestampOption()).flatten.map(_.toInstant))

  // All 41 proto fields + deleted flag = 42 columns
  case class TransactionRow(
    // required fields
    id: String,
    orderUuid: String,
    nature: String,
    transactionType: String,
    status: String,
    amount: Int,
    currency: String,
    fees: Int,
    resultCode: String,
    resultMessage: String,
    authorId: String,
    paymentType: String,
    createdDate: java.time.Instant,
    lastUpdated: java.time.Instant,
    // optional fields
    paymentMethodId: Option[String],
    redirectUrl: Option[String],
    reasonMessage: Option[String],
    creditedWalletId: Option[String],
    creditedUserId: Option[String],
    debitedWalletId: Option[String],
    debitedUserId: Option[String],
    mandateId: Option[String],
    preAuthorizationId: Option[String],
    recurringPayInRegistrationId: Option[String],
    externalReference: Option[String],
    preAuthorizationValidated: Option[Boolean],
    preAuthorizationCanceled: Option[Boolean],
    preAuthorizationExpired: Option[Boolean],
    preAuthorizationDebitedAmount: Option[Int],
    returnUrl: Option[String],
    payPalPayerEmail: Option[String],
    idempotencyKey: Option[String],
    clientId: Option[String],
    paymentClientSecret: Option[String],
    paymentClientData: Option[String],
    paymentClientReturnUrl: Option[String],
    sourceTransactionId: Option[String],
    transferAmount: Option[Int],
    preRegistrationId: Option[String],
    payPalPayerId: Option[String],
    payInId: Option[String],
    // soft delete
    deleted: Boolean
  )

  type RowType = TransactionRow

  class Transactions(tag: Tag) extends Table[RowType](tag, dataset, tableName) {
    def id = column[String]("id", O.PrimaryKey)
    def orderUuid = column[String]("order_uuid")
    def nature = column[String]("nature")
    def transactionType = column[String]("transaction_type")
    def status = column[String]("status")
    def amount = column[Int]("amount")
    def currency = column[String]("currency")
    def fees = column[Int]("fees")
    def resultCode = column[String]("result_code")
    def resultMessage = column[String]("result_message")
    def authorId = column[String]("author_id")
    def paymentType = column[String]("payment_type")
    def createdDate = column[java.time.Instant]("created_date")
    def lastUpdated = column[java.time.Instant]("last_updated")
    def paymentMethodId = column[Option[String]]("payment_method_id")
    def redirectUrl = column[Option[String]]("redirect_url")
    def reasonMessage = column[Option[String]]("reason_message")
    def creditedWalletId = column[Option[String]]("credited_wallet_id")
    def creditedUserId = column[Option[String]]("credited_user_id")
    def debitedWalletId = column[Option[String]]("debited_wallet_id")
    def debitedUserId = column[Option[String]]("debited_user_id")
    def mandateId = column[Option[String]]("mandate_id")
    def preAuthorizationId = column[Option[String]]("pre_authorization_id")
    def recurringPayInRegistrationId = column[Option[String]]("recurring_pay_in_registration_id")
    def externalReference = column[Option[String]]("external_reference")
    def preAuthorizationValidated = column[Option[Boolean]]("pre_authorization_validated")
    def preAuthorizationCanceled = column[Option[Boolean]]("pre_authorization_canceled")
    def preAuthorizationExpired = column[Option[Boolean]]("pre_authorization_expired")
    def preAuthorizationDebitedAmount = column[Option[Int]]("pre_authorization_debited_amount")
    def returnUrl = column[Option[String]]("return_url")
    def payPalPayerEmail = column[Option[String]]("paypal_payer_email")
    def idempotencyKey = column[Option[String]]("idempotency_key")
    def clientId = column[Option[String]]("client_id")
    def paymentClientSecret = column[Option[String]]("payment_client_secret")
    def paymentClientData = column[Option[String]]("payment_client_data")
    def paymentClientReturnUrl = column[Option[String]]("payment_client_return_url")
    def sourceTransactionId = column[Option[String]]("source_transaction_id")
    def transferAmount = column[Option[Int]]("transfer_amount")
    def preRegistrationId = column[Option[String]]("pre_registration_id")
    def payPalPayerId = column[Option[String]]("paypal_payer_id")
    def payInId = column[Option[String]]("pay_in_id")
    def deleted = column[Boolean]("deleted")

    // Split into 4 parts to stay within Scala 2's 22-element tuple limit (42 columns)
    private val part1 = (
      id,
      orderUuid,
      nature,
      transactionType,
      status,
      amount,
      currency,
      fees,
      resultCode,
      resultMessage,
      authorId
    )
    private val part2 = (
      paymentType,
      createdDate,
      lastUpdated,
      paymentMethodId,
      redirectUrl,
      reasonMessage,
      creditedWalletId,
      creditedUserId,
      debitedWalletId,
      debitedUserId,
      mandateId
    )
    private val part3 = (
      preAuthorizationId,
      recurringPayInRegistrationId,
      externalReference,
      preAuthorizationValidated,
      preAuthorizationCanceled,
      preAuthorizationExpired,
      preAuthorizationDebitedAmount,
      returnUrl,
      payPalPayerEmail,
      idempotencyKey,
      clientId
    )
    private val part4 = (
      paymentClientSecret,
      paymentClientData,
      paymentClientReturnUrl,
      sourceTransactionId,
      transferAmount,
      preRegistrationId,
      payPalPayerId,
      payInId,
      deleted
    )
    def * = (part1, part2, part3, part4) <> ({ case (p1, p2, p3, p4) =>
      TransactionRow(
        p1._1,
        p1._2,
        p1._3,
        p1._4,
        p1._5,
        p1._6,
        p1._7,
        p1._8,
        p1._9,
        p1._10,
        p1._11,
        p2._1,
        p2._2,
        p2._3,
        p2._4,
        p2._5,
        p2._6,
        p2._7,
        p2._8,
        p2._9,
        p2._10,
        p2._11,
        p3._1,
        p3._2,
        p3._3,
        p3._4,
        p3._5,
        p3._6,
        p3._7,
        p3._8,
        p3._9,
        p3._10,
        p3._11,
        p4._1,
        p4._2,
        p4._3,
        p4._4,
        p4._5,
        p4._6,
        p4._7,
        p4._8,
        p4._9
      )
    }, { (r: TransactionRow) =>
      Some(
        (
          (
            r.id,
            r.orderUuid,
            r.nature,
            r.transactionType,
            r.status,
            r.amount,
            r.currency,
            r.fees,
            r.resultCode,
            r.resultMessage,
            r.authorId
          ),
          (
            r.paymentType,
            r.createdDate,
            r.lastUpdated,
            r.paymentMethodId,
            r.redirectUrl,
            r.reasonMessage,
            r.creditedWalletId,
            r.creditedUserId,
            r.debitedWalletId,
            r.debitedUserId,
            r.mandateId
          ),
          (
            r.preAuthorizationId,
            r.recurringPayInRegistrationId,
            r.externalReference,
            r.preAuthorizationValidated,
            r.preAuthorizationCanceled,
            r.preAuthorizationExpired,
            r.preAuthorizationDebitedAmount,
            r.returnUrl,
            r.payPalPayerEmail,
            r.idempotencyKey,
            r.clientId
          ),
          (
            r.paymentClientSecret,
            r.paymentClientData,
            r.paymentClientReturnUrl,
            r.sourceTransactionId,
            r.transferAmount,
            r.preRegistrationId,
            r.payPalPayerId,
            r.payInId,
            r.deleted
          )
        )
      )
    })
  }

  type TableType = Transactions
  override def tableQuery: TableQuery[Transactions] = TableQuery[Transactions]

  override def rowUuidColumn(row: Transactions): Rep[String] = row.id

  /** Compatibility method for tests that use the old JdbcStateProvider.load(uuid) API */
  def load(uuid: String): Option[Transaction] = loadDocument(uuid)

  override def toRow(entity: Transaction, deleted: Boolean = false): RowType =
    TransactionRow(
      id = entity.id,
      orderUuid = entity.orderUuid,
      nature = entity.nature.name,
      transactionType = entity.`type`.name,
      status = entity.status.name,
      amount = entity.amount,
      currency = entity.currency,
      fees = entity.fees,
      resultCode = entity.resultCode,
      resultMessage = entity.resultMessage,
      authorId = entity.authorId,
      paymentType = entity.paymentType.name,
      createdDate = entity.createdDate,
      lastUpdated = entity.lastUpdated,
      paymentMethodId = entity.paymentMethodId,
      redirectUrl = entity.redirectUrl,
      reasonMessage = entity.reasonMessage,
      creditedWalletId = entity.creditedWalletId,
      creditedUserId = entity.creditedUserId,
      debitedWalletId = entity.debitedWalletId,
      debitedUserId = entity.debitedUserId,
      mandateId = entity.mandateId,
      preAuthorizationId = entity.preAuthorizationId,
      recurringPayInRegistrationId = entity.recurringPayInRegistrationId,
      externalReference = entity.externalReference,
      preAuthorizationValidated = entity.preAuthorizationValidated,
      preAuthorizationCanceled = entity.preAuthorizationCanceled,
      preAuthorizationExpired = entity.preAuthorizationExpired,
      preAuthorizationDebitedAmount = entity.preAuthorizationDebitedAmount,
      returnUrl = entity.returnUrl,
      payPalPayerEmail = entity.payPalPayerEmail,
      idempotencyKey = entity.idempotencyKey,
      clientId = entity.clientId,
      paymentClientSecret = entity.paymentClientSecret,
      paymentClientData = entity.paymentClientData,
      paymentClientReturnUrl = entity.paymentClientReturnUrl,
      sourceTransactionId = entity.sourceTransactionId,
      transferAmount = entity.transferAmount,
      preRegistrationId = entity.preRegistrationId,
      payPalPayerId = entity.payPalPayerId,
      payInId = entity.payInId,
      deleted = deleted
    )

  override def fromRow(row: RowType): Option[Transaction] = {
    if (row.deleted) None
    else
      Some(
        Transaction(
          id = row.id,
          orderUuid = row.orderUuid,
          nature = Transaction.TransactionNature
            .fromName(row.nature)
            .getOrElse(Transaction.TransactionNature.REGULAR),
          `type` = Transaction.TransactionType
            .fromName(row.transactionType)
            .getOrElse(Transaction.TransactionType.PAYIN),
          status = Transaction.TransactionStatus
            .fromName(row.status)
            .getOrElse(Transaction.TransactionStatus.TRANSACTION_CREATED),
          amount = row.amount,
          currency = row.currency,
          fees = row.fees,
          resultCode = row.resultCode,
          resultMessage = row.resultMessage,
          authorId = row.authorId,
          paymentType = Transaction.PaymentType
            .fromName(row.paymentType)
            .getOrElse(Transaction.PaymentType.CARD),
          createdDate = row.createdDate,
          lastUpdated = row.lastUpdated,
          paymentMethodId = row.paymentMethodId,
          redirectUrl = row.redirectUrl,
          reasonMessage = row.reasonMessage,
          creditedWalletId = row.creditedWalletId,
          creditedUserId = row.creditedUserId,
          debitedWalletId = row.debitedWalletId,
          debitedUserId = row.debitedUserId,
          mandateId = row.mandateId,
          preAuthorizationId = row.preAuthorizationId,
          recurringPayInRegistrationId = row.recurringPayInRegistrationId,
          externalReference = row.externalReference,
          preAuthorizationValidated = row.preAuthorizationValidated,
          preAuthorizationCanceled = row.preAuthorizationCanceled,
          preAuthorizationExpired = row.preAuthorizationExpired,
          preAuthorizationDebitedAmount = row.preAuthorizationDebitedAmount,
          returnUrl = row.returnUrl,
          payPalPayerEmail = row.payPalPayerEmail,
          idempotencyKey = row.idempotencyKey,
          clientId = row.clientId,
          paymentClientSecret = row.paymentClientSecret,
          paymentClientData = row.paymentClientData,
          paymentClientReturnUrl = row.paymentClientReturnUrl,
          sourceTransactionId = row.sourceTransactionId,
          transferAmount = row.transferAmount,
          preRegistrationId = row.preRegistrationId,
          payPalPayerId = row.payPalPayerId,
          payInId = row.payInId
        )
      )
  }

  implicit val getResult: GetResult[RowType] = GetResult { r =>
    TransactionRow(
      id = r.<<,
      orderUuid = r.<<,
      nature = r.<<,
      transactionType = r.<<,
      status = r.<<,
      amount = r.<<,
      currency = r.<<,
      fees = r.<<,
      resultCode = r.<<,
      resultMessage = r.<<,
      authorId = r.<<,
      paymentType = r.<<,
      createdDate = r.<<,
      lastUpdated = r.<<,
      paymentMethodId = r.<<?,
      redirectUrl = r.<<?,
      reasonMessage = r.<<?,
      creditedWalletId = r.<<?,
      creditedUserId = r.<<?,
      debitedWalletId = r.<<?,
      debitedUserId = r.<<?,
      mandateId = r.<<?,
      preAuthorizationId = r.<<?,
      recurringPayInRegistrationId = r.<<?,
      externalReference = r.<<?,
      preAuthorizationValidated = r.<<?,
      preAuthorizationCanceled = r.<<?,
      preAuthorizationExpired = r.<<?,
      preAuthorizationDebitedAmount = r.<<?,
      returnUrl = r.<<?,
      payPalPayerEmail = r.<<?,
      idempotencyKey = r.<<?,
      clientId = r.<<?,
      paymentClientSecret = r.<<?,
      paymentClientData = r.<<?,
      paymentClientReturnUrl = r.<<?,
      sourceTransactionId = r.<<?,
      transferAmount = r.<<?,
      preRegistrationId = r.<<?,
      payPalPayerId = r.<<?,
      payInId = r.<<?,
      deleted = r.<<
    )
  }
}
