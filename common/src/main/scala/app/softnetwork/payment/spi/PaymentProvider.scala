package app.softnetwork.payment.spi

import app.softnetwork.payment.model._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import java.util.Date

/** Created by smanciot on 16/08/2018.
  */
private[payment] trait PaymentProvider {

  protected lazy val mlog: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  implicit def provider: SoftPayAccount.Client.Provider

  /** @param maybePaymentAccount
    *   - payment account to create or update
    * @return
    *   provider user id
    */
  def createOrUpdatePaymentAccount(maybePaymentAccount: Option[PaymentAccount]): Option[String] = {
    maybePaymentAccount match {
      case Some(paymentAccount) =>
        import paymentAccount._
        if (user.isLegalUser) {
          createOrUpdateLegalUser(user.legalUser)
        } else if (user.isNaturalUser) {
          createOrUpdateNaturalUser(user.naturalUser)
        } else {
          None
        }
      case _ => None
    }
  }

  /** @param maybeNaturalUser
    *   - natural user to create
    * @return
    *   provider user id
    */
  def createOrUpdateNaturalUser(maybeNaturalUser: Option[NaturalUser]): Option[String]

  /** @param maybeLegalUser
    *   - legal user to create
    * @return
    *   provider user id
    */
  def createOrUpdateLegalUser(maybeLegalUser: Option[LegalUser]): Option[String]

  /** @param maybeUserId
    *   - owner of the wallet
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @param maybeWalletId
    *   - wallet id to update
    * @return
    *   wallet id
    */
  def createOrUpdateWallet(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String,
    maybeWalletId: Option[String]
  ): Option[String]

  /** @param maybeBankAccount
    *   - bank account to create
    * @return
    *   bank account id
    */
  def createOrUpdateBankAccount(maybeBankAccount: Option[BankAccount]): Option[String]

  /** @param userId
    *   - provider user id
    * @return
    *   the first active bank account
    */
  def getActiveBankAccount(userId: String): Option[String]

  /** @param userId
    *   - provider user id
    * @param bankAccountId
    *   - bank account id
    * @return
    *   whether this bank account exists and is active
    */
  def checkBankAccount(userId: String, bankAccountId: String): Boolean

  /** @param maybeUserId
    *   - owner of the card
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @return
    *   card pre registration
    */
  def preRegisterCard(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String
  ): Option[CardPreRegistration]

  /** @param cardPreRegistrationId
    *   - card registration id
    * @param maybeRegistrationData
    *   - card registration data
    * @return
    *   card id
    */
  def createCard(
    cardPreRegistrationId: String,
    maybeRegistrationData: Option[String]
  ): Option[String]

  /** @param cardId
    *   - card id
    * @return
    *   card
    */
  def loadCard(cardId: String): Option[Card]

  /** @param cardId
    *   - the id of the card to disable
    * @return
    *   the card disabled or none
    */
  def disableCard(cardId: String): Option[Card]

  /** @param preAuthorizationTransaction
    *   - pre authorization transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pre authorization transaction result
    */
  def preAuthorizeCard(
    preAuthorizationTransaction: PreAuthorizationTransaction,
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   card pre authorized transaction
    */
  def loadCardPreAuthorized(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Option[Transaction]

  /** @param payInWithCardPreAuthorizedTransaction
    *   - card pre authorized pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with card pre authorized transaction result
    */
  def payInWithCardPreAuthorized(
    payInWithCardPreAuthorizedTransaction: PayInWithCardPreAuthorizedTransaction,
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been cancelled or not
    */
  def cancelPreAuthorization(orderUuid: String, cardPreAuthorizedTransactionId: String): Boolean

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been validated or not
    */
  def validatePreAuthorization(orderUuid: String, cardPreAuthorizedTransactionId: String): Boolean

  /** @param maybePayInTransaction
    *   - pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in transaction result
    */
  def payIn(
    maybePayInTransaction: Option[PayInTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param payInWithPayPalTransaction
    *   - pay in with PayPal transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with PayPal transaction result
    */
  def payInWithPayPal(
    payInWithPayPalTransaction: PayInWithPayPalTransaction,
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param maybeRefundTransaction
    *   - refund transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   refund transaction result
    */
  def refund(
    maybeRefundTransaction: Option[RefundTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param maybeTransferTransaction
    *   - transfer transaction
    * @return
    *   transfer transaction result
    */
  def transfer(maybeTransferTransaction: Option[TransferTransaction]): Option[Transaction]

  /** @param maybePayOutTransaction
    *   - pay out transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay out transaction result
    */
  def payOut(
    maybePayOutTransaction: Option[PayOutTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay in transaction
    */
  def loadPayIn(
    orderUuid: String,
    transactionId: String,
    recurringPayInRegistrationId: Option[String]
  ): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   Refund transaction
    */
  def loadRefund(orderUuid: String, transactionId: String): Option[Transaction]

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay out transaction
    */
  def loadPayOut(orderUuid: String, transactionId: String): Option[Transaction]

  /** @param transactionId
    *   - transaction id
    * @return
    *   transfer transaction
    */
  def loadTransfer(transactionId: String): Option[Transaction]

  /** @param userId
    *   - Provider user id
    * @param externalUuid
    *   - external unique id
    * @param pages
    *   - document pages
    * @param documentType
    *   - document type
    * @return
    *   Provider document id
    */
  def addDocument(
    userId: String,
    externalUuid: String,
    pages: Seq[Array[Byte]],
    documentType: KycDocument.KycDocumentType
  ): Option[String]

  /** @param userId
    *   - Provider user id
    * @param documentId
    *   - Provider document id
    * @return
    *   document validation report
    */
  def loadDocumentStatus(userId: String, documentId: String): KycDocumentValidationReport

  /** @param externalUuid
    *   - external unique id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - Bank account id
    * @param idempotencyKey
    *   - whether to use an idempotency key for this request or not
    * @return
    *   mandate result
    */
  def mandate(
    externalUuid: String,
    userId: String,
    bankAccountId: String,
    idempotencyKey: Option[String] = None
  ): Option[MandateResult]

  /** @param maybeMandateId
    *   - optional mandate id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - bank account id
    * @return
    *   mandate associated to this bank account
    */
  def loadMandate(
    maybeMandateId: Option[String],
    userId: String,
    bankAccountId: String
  ): Option[MandateResult]

  /** @param mandateId
    *   - Provider mandate id
    * @return
    *   mandate result
    */
  def cancelMandate(mandateId: String): Option[MandateResult]

  /** @param maybeDirectDebitTransaction
    *   - direct debit transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   direct debit transaction result
    */
  def directDebit(
    maybeDirectDebitTransaction: Option[DirectDebitTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction]

  /** @param walletId
    *   - Provider wallet id
    * @param transactionId
    *   - Provider transaction id
    * @param transactionDate
    *   - Provider transaction date
    * @return
    *   transaction if it exists
    */
  def directDebitTransaction(
    walletId: String,
    transactionId: String,
    transactionDate: Date
  ): Option[Transaction]

  /** @return
    *   client fees
    */
  def client: Option[SoftPayAccount.Client]

  def clientFees(): Option[Double]

  /** @param userId
    *   - Provider user id
    * @return
    *   Ultimate Beneficial Owner Declaration
    */
  def createDeclaration(userId: String): Option[UboDeclaration]

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @param ultimateBeneficialOwner
    *   - Ultimate Beneficial Owner
    * @return
    *   Ultimate Beneficial Owner created or updated
    */
  def createOrUpdateUBO(
    userId: String,
    uboDeclarationId: String,
    ultimateBeneficialOwner: UboDeclaration.UltimateBeneficialOwner
  ): Option[UboDeclaration.UltimateBeneficialOwner]

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   declaration with Ultimate Beneficial Owner(s)
    */
  def getDeclaration(userId: String, uboDeclarationId: String): Option[UboDeclaration]

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   Ultimate Beneficial Owner declaration
    */
  def validateDeclaration(userId: String, uboDeclarationId: String): Option[UboDeclaration]

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
  def registerRecurringCardPayment(
    userId: String,
    walletId: String,
    cardId: String,
    recurringPayment: RecurringPayment
  ): Option[RecurringPayment.RecurringCardPaymentResult] = None

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @param cardId
    *   - Provider card id
    * @param status
    *   - optional recurring payment status
    * @return
    *   recurring card payment registration updated result
    */
  def updateRecurringCardPaymentRegistration(
    recurringPayInRegistrationId: String,
    cardId: Option[String],
    status: Option[RecurringPayment.RecurringCardPaymentStatus]
  ): Option[RecurringPayment.RecurringCardPaymentResult] = None

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @return
    *   recurring card payment registration result
    */
  def loadRecurringCardPayment(
    recurringPayInRegistrationId: String
  ): Option[RecurringPayment.RecurringCardPaymentResult] = None

  /** @param recurringPaymentTransaction
    *   - recurring payment transaction
    * @return
    *   resulted payIn transaction
    */
  def createRecurringCardPayment(
    recurringPaymentTransaction: RecurringPaymentTransaction
  ): Option[Transaction] = None

}
