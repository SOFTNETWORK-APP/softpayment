package app.softnetwork.payment.message

import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.message.PaymentEvents.{
  ExternalEntityToPaymentEvent,
  PaymentCommandEvent,
  PaymentEventWithCommand
}
import app.softnetwork.payment.model._
import app.softnetwork.persistence.message.{Command, CommandResult, EntityCommand, ErrorMessage}
import app.softnetwork.scheduler.model.Schedule

import java.util.Date

object PaymentMessages {
  trait PaymentCommand extends Command

  trait PaymentCommandWithKey extends PaymentCommand {
    def key: String
  }

  trait PaymentAccountCommand extends PaymentCommand

  /** Payment Commands */

  trait CardCommand extends PaymentCommand

  /** @param orderUuid
    *   - order uuid
    * @param user
    *   - payment user
    * @param currency
    *   - currency
    * @param clientId
    *   - optional client id
    */
  case class PreRegisterCard(
    orderUuid: String,
    user: NaturalUser,
    currency: String = "EUR",
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with CardCommand {
    val key: String = user.externalUuidWithProfile
  }

  /** @param orderUuid
    *   - order unique id
    * @param debitedAmount
    *   - debited amount in cents
    * @param currency
    *   - currency
    * @param registrationId
    *   - card registration id
    * @param registrationData
    *   - card registration data
    * @param registerCard
    *   - register card
    * @param javaEnabled
    *   - java enabled
    * @param javascriptEnabled
    *   - javascript enabled
    * @param colorDepth
    *   - color depth
    * @param screenWidth
    *   - screen width
    * @param screenHeight
    *   - screen height
    * @param paymentType
    *   - payment type
    * @param printReceipt
    *   - whether or not the client asks to print a receipt
    * @param feesAmount
    *   - optional fees amount
    * @param user
    *   - optional payment user
    */
  case class Payment(
    orderUuid: String,
    debitedAmount: Int = 100,
    currency: String = "EUR",
    registrationId: Option[String] = None,
    registrationData: Option[String] = None,
    registerCard: Boolean = false,
    javaEnabled: Boolean = false,
    javascriptEnabled: Boolean = true,
    colorDepth: Option[Int] = None,
    screenWidth: Option[Int] = None,
    screenHeight: Option[Int] = None,
    statementDescriptor: Option[String] = None,
    paymentType: Transaction.PaymentType = Transaction.PaymentType.CARD,
    printReceipt: Boolean = false,
    feesAmount: Option[Int] = None,
    user: Option[NaturalUser] = None
  )

  /** Flow [PreRegisterCard -> ] PreAuthorizeCard [ -> PreAuthorizeCardCallback]
    *
    * @param orderUuid
    *   - order uuid
    * @param debitedAccount
    *   - account to debit
    * @param debitedAmount
    *   - amount to debit from the debited account
    * @param currency
    *   - currency
    * @param registrationId
    *   - card registration id
    * @param registrationData
    *   - card registration data
    * @param registerCard
    *   - register card
    * @param ipAddress
    *   - ip address
    * @param browserInfo
    *   - browser info
    * @param printReceipt
    *   - whether or not the client asks to print a receipt
    * @param creditedAccount
    *   - account to credit
    * @param feesAmount
    *   - optional fees amount
    */
  case class PreAuthorizeCard(
    orderUuid: String,
    debitedAccount: String,
    debitedAmount: Int = 100,
    currency: String = "EUR",
    registrationId: Option[String] = None,
    registrationData: Option[String] = None,
    registerCard: Boolean = false,
    ipAddress: Option[String] = None,
    browserInfo: Option[BrowserInfo] = None,
    printReceipt: Boolean = false,
    creditedAccount: Option[String] = None,
    feesAmount: Option[Int] = None
  ) extends PaymentCommandWithKey
      with CardCommand {
    val key: String = debitedAccount
  }

  /** card pre authorization return command
    *
    * @param orderUuid
    *   - order unique id
    * @param preAuthorizationId
    *   - pre authorization transaction id
    * @param registerCard
    *   - whether the card should be registered or not
    * @param printReceipt
    *   - whether or not the client asks to print a receipt
    */
  @InternalApi
  private[payment] case class PreAuthorizeCardCallback(
    orderUuid: String,
    preAuthorizationId: String,
    registerCard: Boolean = true,
    printReceipt: Boolean = false
  ) extends PaymentCommandWithKey
      with CardCommand {
    lazy val key: String = preAuthorizationId
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @param clientId
    *   - optional client id
    */
  case class CancelPreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with CardCommand {
    lazy val key: String = cardPreAuthorizedTransactionId
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    */
  case class ValidatePreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ) extends PaymentCommandWithKey
      with CardCommand {
    lazy val key: String = cardPreAuthorizedTransactionId
  }

  trait PayInCommand extends PaymentCommand

  /** Flow [PreRegisterCard -> ] PreAuthorizeCard [ -> PreAuthorizeCardCallback] ->
    * PayInWithCardPreAuthorized
    *
    * @param preAuthorizationId
    *   - pre authorization transaction id
    * @param creditedAccount
    *   - account to credit
    * @param debitedAmount
    *   - amount to be debited from the account that made the pre-authorization (if not specified it
    *     will be the amount specified during the pre-authorization)
    * @param feesAmount
    *   - optional fees amount
    * @param clientId
    *   - optional client id
    */
  case class PayInWithCardPreAuthorized(
    preAuthorizationId: String,
    creditedAccount: String,
    debitedAmount: Option[Int],
    feesAmount: Option[Int] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PayInCommand
      with CardCommand {
    lazy val key: String = preAuthorizationId
  }

  /** Flow [PreRegisterCard ->] PayIn [ -> PayInCallback]
    *
    * @param orderUuid
    *   - order uuid
    * @param debitedAccount
    *   - account to debit
    * @param debitedAmount
    *   - amount to be debited from the debited account
    * @param currency
    *   - currency
    * @param creditedAccount
    *   - account to credit
    * @param registrationId
    *   - card registration id
    * @param registrationData
    *   - card registration data
    * @param registerCard
    *   - register card
    * @param ipAddress
    *   - ip address
    * @param browserInfo
    *   - browser info
    * @param paymentType
    *   - payment type
    * @param printReceipt
    *   - whether or not the client asks to print a receipt
    * @param feesAmount
    *   - optional fees amount
    * @param user
    *   - optional payment user
    */
  case class PayIn(
    orderUuid: String,
    debitedAccount: String,
    debitedAmount: Int,
    currency: String = "EUR",
    creditedAccount: String,
    registrationId: Option[String] = None,
    registrationData: Option[String] = None,
    registerCard: Boolean = false,
    ipAddress: Option[String] = None,
    browserInfo: Option[BrowserInfo] = None,
    statementDescriptor: Option[String] = None,
    paymentType: Transaction.PaymentType = Transaction.PaymentType.CARD,
    printReceipt: Boolean = false,
    feesAmount: Option[Int] = None,
    user: Option[NaturalUser] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PayInCommand {
    val key: String = debitedAccount
  }

  /** pay in return command
    *
    * @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - payIn transaction id
    * @param registerCard
    *   - the card should be registered or not
    * @param printReceipt
    *   - whether or not the client asks to print a receipt
    */
  @InternalApi
  private[payment] case class PayInCallback(
    orderUuid: String,
    transactionId: String,
    registerCard: Boolean,
    printReceipt: Boolean = false
  ) extends PaymentCommandWithKey
      with PayInCommand {
    lazy val key: String = transactionId
  }

  trait PayOutCommand extends PaymentCommand

  /** @param orderUuid
    *   - order uuid
    * @param creditedAccount
    *   - account to credit
    * @param creditedAmount
    *   - credited amount
    * @param feesAmount
    *   - fees amount
    * @param currency
    *   - currency
    * @param externalReference
    *   - optional external reference
    * @param payInTransactionId
    *   - optional payIn transaction id
    * @param clientId
    *   - optional client id
    */
  case class PayOut(
    orderUuid: String,
    creditedAccount: String,
    creditedAmount: Int,
    feesAmount: Int = 0,
    currency: String = "EUR",
    externalReference: Option[String] = None,
    payInTransactionId: Option[String] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PayOutCommand {
    val key: String = creditedAccount
  }

  case class LoadPayOutTransaction(
    orderUuid: String,
    transactionId: String,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PayOutCommand {
    val key: String = transactionId
  }

  case class LoadPayInTransaction(
    orderUuid: String,
    transactionId: String,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PayInCommand {
    val key: String = transactionId
  }

  /** @param orderUuid
    *   - order uuid
    * @param payInTransactionId
    *   - payIn transaction id
    * @param refundAmount
    *   - refund amount
    * @param feesRefundAmount
    *   - fees refund amount
    * @param currency
    *   - currency
    * @param reasonMessage
    *   - reason message
    * @param initializedByClient
    *   - whether the refund is initialized by the client or not
    * @param clientId
    *   - optional client id
    */
  case class Refund(
    orderUuid: String,
    payInTransactionId: String,
    refundAmount: Int,
    feesRefundAmount: Option[Int] = None,
    currency: String = "EUR",
    reasonMessage: String,
    initializedByClient: Boolean,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey {
    lazy val key: String = payInTransactionId
  }

  /** @param orderUuid
    *   - optional order uuid
    * @param debitedAccount
    *   - debited account
    * @param creditedAccount
    *   - credited account
    * @param debitedAmount
    *   - debited amount
    * @param feesAmount
    *   - fees amount
    * @param currency
    *   - currency
    * @param payOutRequired
    *   - whether an immediate pay out is required or not
    * @param externalReference
    *   - optional external reference
    * @param clientId
    *   - optional client id
    */
  case class Transfer(
    orderUuid: Option[String] = None,
    debitedAccount: String,
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int = 0,
    currency: String = "EUR",
    payOutRequired: Boolean = true,
    externalReference: Option[String] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey {
    val key: String = debitedAccount
  }

  /** @param creditedAccount
    *   - account whose wallet will be credited through the bank wire direct debit
    * @param debitedAmount
    *   - debited amount
    * @param feesAmount
    *   - fees amount
    * @param currency
    *   - currency
    * @param statementDescriptor
    *   - statement descriptor
    * @param externalReference
    *   - optional external reference
    * @param clientId
    *   - optional client id
    */
  case class DirectDebit(
    creditedAccount: String,
    debitedAmount: Int,
    feesAmount: Int = 0,
    currency: String = "EUR",
    statementDescriptor: String,
    externalReference: Option[String] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey {
    val key: String = creditedAccount
  }

  /** @param directDebitTransactionId
    *   - direct debit transaction id
    * @param clientId
    *   - optional client id
    */
  case class LoadDirectDebitTransaction(
    directDebitTransactionId: String,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey {
    val key: String = directDebitTransactionId
  }

  trait RecurringPaymentCommand extends PaymentCommand

  /** @param debitedAccount
    *   - account to debit
    * @param firstDebitedAmount
    *   - first debited amount
    * @param firstFeesAmount
    *   - first fees amount
    * @param currency
    *   - currency
    * @param `type`
    *   - recurring payment type
    * @param startDate
    *   - recurring payment start date
    * @param endDate
    *   - recurring payment end date
    * @param frequency
    *   - recurring payment frequency
    * @param fixedNextAmount
    *   - whether next payments are fixed or not
    * @param nextDebitedAmount
    *   - next debited amount
    * @param nextFeesAmount
    *   - next fees amount
    * @param statementDescriptor
    *   - statement descriptor
    * @param externalReference
    *   - optional external reference
    * @param clientId
    *   - optional client id
    */
  case class RegisterRecurringPayment(
    debitedAccount: String,
    firstDebitedAmount: Int = 0,
    firstFeesAmount: Int = 0,
    currency: String = "EUR",
    `type`: RecurringPayment.RecurringPaymentType = RecurringPayment.RecurringPaymentType.CARD,
    startDate: Option[Date] = None,
    endDate: Option[Date] = None,
    frequency: Option[RecurringPayment.RecurringPaymentFrequency] = None,
    fixedNextAmount: Option[Boolean] = None,
    nextDebitedAmount: Option[Int] = None,
    nextFeesAmount: Option[Int] = None,
    statementDescriptor: Option[String] = None,
    externalReference: Option[String] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with RecurringPaymentCommand {
    val key: String = debitedAccount
  }

  /** @param recurringPaymentRegistrationId
    *   - recurring payment registration id
    * @param cardId
    *   - card id
    * @param status
    *   - recurring payIn registration status
    */
  case class UpdateRecurringCardPaymentRegistration(
    debitedAccount: String,
    recurringPaymentRegistrationId: String,
    cardId: Option[String] = None,
    status: Option[RecurringPayment.RecurringCardPaymentStatus] = None
  ) extends PaymentCommandWithKey
      with RecurringPaymentCommand {
    val key: String = debitedAccount
  }

  /** @param recurringPaymentRegistrationId
    *   - recurring payment registration id
    */
  case class LoadRecurringPayment(debitedAccount: String, recurringPaymentRegistrationId: String)
      extends PaymentCommandWithKey
      with RecurringPaymentCommand {
    val key: String = debitedAccount
  }

  /** @param recurringPaymentRegistrationId
    *   - recurring payment registration id
    * @param debitedAccount
    *   - debited account
    * @param ipAddress
    *   - ip address
    * @param browserInfo
    *   - browser info
    * @param statementDescriptor
    *   - statement descriptor
    */
  case class ExecuteFirstRecurringPayment(
    recurringPaymentRegistrationId: String,
    debitedAccount: String,
    ipAddress: Option[String] = None,
    browserInfo: Option[BrowserInfo] = None,
    statementDescriptor: Option[String] = None
  ) extends PaymentCommandWithKey
      with RecurringPaymentCommand {
    val key: String = debitedAccount
  }

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @param transactionId
    *   - transaction payIn id
    */
  @InternalApi
  private[payment] case class FirstRecurringPaymentCallback(
    recurringPayInRegistrationId: String,
    transactionId: String
  ) extends PaymentCommandWithKey
      with RecurringPaymentCommand {
    lazy val key: String = transactionId
  }

  /** @param recurringPaymentRegistrationId
    *   - recurring payment registration id
    * @param debitedAccount
    *   - debited account
    * @param nextDebitedAmount
    *   - next debited amount
    * @param nextFeesAmount
    *   - next fees amount
    * @param statementDescriptor
    *   - statement descriptor
    */
  case class ExecuteNextRecurringPayment(
    recurringPaymentRegistrationId: String,
    debitedAccount: String,
    nextDebitedAmount: Option[Int] = None,
    nextFeesAmount: Option[Int] = None,
    statementDescriptor: Option[String] = None
  ) extends PaymentCommandWithKey
      with RecurringPaymentCommand {
    val key: String = debitedAccount
  }

  case class TriggerSchedule4Payment(schedule: Schedule) extends PaymentCommand with EntityCommand {
    override def id: String = schedule.entityId
  }

  /** Commands related to the payment account */

  /** private api command
    *
    * @param account
    *   - payment account reference
    * @param clientId
    *   - optional client id
    */
  @InternalApi
  private[payment] case class LoadPaymentAccount(account: String, clientId: Option[String] = None)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = account
  }

  /** @param transactionId
    *   - transaction id
    */
  case class LoadTransaction(transactionId: String)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = transactionId
  }

  /** Commands related to the bank account */

  case class BankAccountCommand(
    bankAccount: BankAccount,
    user: Either[NaturalUser, LegalUser],
    acceptedTermsOfPSP: Option[Boolean] = None
  )

  object BankAccountCommand {

    def apply(
      bankAccount: BankAccount,
      naturalUser: NaturalUser,
      acceptedTermsOfPSP: Option[Boolean]
    ): BankAccountCommand = BankAccountCommand(bankAccount, Left(naturalUser), acceptedTermsOfPSP)

    def apply(
      bankAccount: BankAccount,
      legalUser: LegalUser,
      acceptedTermsOfPSP: Option[Boolean]
    ): BankAccountCommand = BankAccountCommand(bankAccount, Right(legalUser), acceptedTermsOfPSP)
  }

  /** @param creditedAccount
    *   - account to credit
    * @param bankAccount
    *   - bank account
    * @param user
    *   - payment user
    * @param acceptedTermsOfPSP
    *   - whether or not the terms of the psp are accepted
    * @param ipAddress
    *   - ip address
    * @param userAgent
    *   - user agent
    * @param clientId
    *   - optional client id
    */
  case class CreateOrUpdateBankAccount(
    creditedAccount: String,
    bankAccount: BankAccount,
    user: Option[PaymentAccount.User] = None,
    acceptedTermsOfPSP: Option[Boolean] = None,
    ipAddress: Option[String] = None,
    userAgent: Option[String],
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** @param creditedAccount
    *   - account to load
    * @param clientId
    *   - optional client id
    */
  case class LoadBankAccount(creditedAccount: String, clientId: Option[String] = None)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** @param creditedAccount
    *   - account to delete
    * @param force
    *   - whether or not the bank account should be deleted even if the bank account deletion has
    *     been disabled
    */
  case class DeleteBankAccount(
    creditedAccount: String,
    force: Option[Boolean]
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** @param debitedAccount
    *   - account owning the cards to load
    */
  case class LoadCards(debitedAccount: String) extends PaymentCommandWithKey with CardCommand {
    val key: String = debitedAccount
  }

  /** @param debitedAccount
    *   - account owning the card to disable
    * @param cardId
    *   - card id
    */
  case class DisableCard(debitedAccount: String, cardId: String)
      extends PaymentCommandWithKey
      with CardCommand {
    val key: String = debitedAccount
  }

  /** Commands related to the kyc documents */

  /** @param creditedAccount
    *   - account to whom the KYC document would be added
    * @param pages
    *   - pages of the KYC document
    */
  case class AddKycDocument(
    creditedAccount: String,
    pages: Seq[Array[Byte]],
    kycDocumentType: KycDocument.KycDocumentType
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  @InternalApi
  private[payment] case class CreateOrUpdateKycDocument(
    creditedAccount: String,
    kycDocument: KycDocument
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** hook command
    *
    * @param kycDocumentId
    *   - kyc document id
    * @param status
    *   - kyc document status
    */
  @InternalApi
  private[payment] case class UpdateKycDocumentStatus(
    kycDocumentId: String,
    status: Option[KycDocument.KycDocumentStatus] = None
  ) extends PaymentCommandWithKey {
    lazy val key: String = kycDocumentId
  }

  /** @param creditedAccount
    *   - account which owns the KYC document that would be loaded
    * @param kycDocumentType
    *   - KYC document type
    */
  case class LoadKycDocumentStatus(
    creditedAccount: String,
    kycDocumentType: KycDocument.KycDocumentType
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** Commands related to the ubo declaration */

  /** @param creditedAccount
    *   - account to whom the UBO declaration would be added
    * @param ubo
    *   - ultimate beneficial owner
    */
  case class CreateOrUpdateUbo(
    creditedAccount: String,
    ubo: UboDeclaration.UltimateBeneficialOwner
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** @param creditedAccount
    *   - account which owns the UBO declaration that would be validated
    */
  case class ValidateUboDeclaration(creditedAccount: String, ipAddress: String, userAgent: String)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** @param creditedAccount
    *   - account which owns the UBO declaration that would be loaded
    */
  case class GetUboDeclaration(creditedAccount: String)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = creditedAccount
  }

  /** hook command
    *
    * @param uboDeclarationId
    *   - ubo declaration id
    * @param status
    *   - ubo declaration status
    */
  @InternalApi
  private[payment] case class UpdateUboDeclarationStatus(
    uboDeclarationId: String,
    status: Option[UboDeclaration.UboDeclarationStatus] = None
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = uboDeclarationId
  }

  /** Commands related to the mandate */

  /** @param debitedAccount
    *   - account to whom the mandate would be added
    * @param iban
    *   - optional IBAN
    * @param clientId
    *   - optional client id
    */
  case class CreateMandate(
    debitedAccount: String,
    iban: Option[String] = None,
    clientId: Option[String] = None
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = debitedAccount
  }

  case class IbanMandate(iban: String)

  /** @param debitedAccount
    *   - account which owns the mandate that would be canceled
    * @param clientId
    *   - optional client id
    */
  case class CancelMandate(debitedAccount: String, clientId: Option[String] = None)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    val key: String = debitedAccount
  }

  /** hook command
    *
    * @param mandateId
    *   - mandate id
    * @param status
    *   - mandate status
    */
  @InternalApi
  private[payment] case class UpdateMandateStatus(
    mandateId: String,
    status: Option[Mandate.MandateStatus] = None
  ) extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = mandateId
  }

  /** hook command
    *
    * @param userId
    *   - user id
    */
  @InternalApi
  private[payment] case class ValidateRegularUser(userId: String)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = userId
  }

  /** hook command
    *
    * @param userId
    *   - user id
    */
  @InternalApi
  private[payment] case class InvalidateRegularUser(userId: String)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = userId
  }

  /** @param paymentAccount
    *   - payment account
    */
  @InternalApi
  private[payment] case class CreateOrUpdatePaymentAccount(paymentAccount: PaymentAccount)
      extends PaymentCommandWithKey
      with PaymentAccountCommand {
    lazy val key: String = paymentAccount.externalUuidWithProfile
  }

  trait PaymentResult extends CommandResult

  case class CardPreRegistered(cardPreRegistration: CardPreRegistration) extends PaymentResult

  case class CardPreAuthorized(transactionId: String) extends PaymentResult

  trait PaidInResult extends PaymentResult

  case class PaidIn(transactionId: String, transactionStatus: Transaction.TransactionStatus)
      extends PaidInResult

  case class PaidOut(transactionId: String, transactionStatus: Transaction.TransactionStatus)
      extends PaymentResult

  case class Refunded(transactionId: String, transactionStatus: Transaction.TransactionStatus)
      extends PaymentResult

  case class Transferred(
    transferredTransactionId: String,
    transferredTransactionStatus: Transaction.TransactionStatus,
    paidOutTransactionId: Option[String] = None
  ) extends PaymentResult

  case class DirectDebited(transactionId: String, transactionStatus: Transaction.TransactionStatus)
      extends PaymentResult

  case class PaymentRedirection(redirectUrl: String) extends PaidInResult

  case class PaymentRequired(
    transactionId: String,
    paymentClientSecret: String,
    paymentClientData: String,
    paymentClientReturnUrl: String
  ) extends PaidInResult

  case class MandateRequired(
    mandateId: String,
    mandateClientSecret: String,
    mandateClientData: String,
    mandateClientReturnUrl: String
  ) extends PaymentResult

  case class RecurringPaymentRegistered(recurringPaymentRegistrationId: String)
      extends PaymentResult

  case class RecurringCardPaymentRegistrationUpdated(
    result: RecurringPayment.RecurringCardPaymentResult
  ) extends PaymentResult

  case class RecurringPaymentLoaded(recurringPayment: RecurringPayment) extends PaymentResult

  case class FirstRecurringPaidIn(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus
  ) extends PaidInResult

  case class NextRecurringPaid(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus
  ) extends PaidInResult

  case class PreAuthorizationCanceled(preAuthorizationCanceled: Boolean) extends PaymentResult

  case class PreAuthorizationValidated(preAuthorizationValidated: Boolean) extends PaymentResult

  case class Schedule4PaymentTriggered(schedule: Schedule) extends PaymentResult

  case class PaymentAccountLoaded(paymentAccount: PaymentAccount) extends PaymentResult

  case class BankAccountCreatedOrUpdated(
    userCreated: Boolean,
    userTypeUpdated: Boolean,
    kycUpdated: Boolean,
    userUpdated: Boolean,
    bankAccountCreated: Boolean,
    bankAccountUpdated: Boolean,
    documentsUpdated: Boolean,
    mandateCanceled: Boolean,
    uboDeclarationCreated: Boolean,
    paymentAccount: PaymentAccountView
  ) extends PaymentResult

  case object MandateCreated extends PaymentResult

  case class MandateConfirmationRequired(redirectUrl: String) extends PaymentResult

  case object MandateCanceled extends PaymentResult

  case class MandateStatusUpdated(result: MandateResult) extends PaymentResult

  case class KycDocumentAdded(kycDocumentId: String) extends PaymentResult

  case object KycDocumentCreatedOrUpdated extends PaymentResult

  case class KycDocumentStatusUpdated(report: KycDocumentValidationReport) extends PaymentResult

  case class KycDocumentStatusLoaded(report: KycDocumentValidationReport) extends PaymentResult

  case class UboCreatedOrUpdated(ubo: UboDeclaration.UltimateBeneficialOwner) extends PaymentResult

  case object UboDeclarationAskedForValidation extends PaymentResult

  case class UboDeclarationLoaded(declaration: UboDeclaration) extends PaymentResult

  case object UboDeclarationStatusUpdated extends PaymentResult

  case object RegularUserValidated extends PaymentResult

  case object RegularUserInvalidated extends PaymentResult

  case class BankAccountLoaded(bankAccount: BankAccount) extends PaymentResult

  case object BankAccountDeleted extends PaymentResult

  case class TransactionLoaded(transaction: Transaction) extends PaymentResult

  case class CardsLoaded(cards: Seq[Card]) extends PaymentResult

  case object CardDisabled extends PaymentResult

  case object PaymentAccountCreated extends PaymentResult

  case object PaymentAccountUpdated extends PaymentResult

  case class PayInTransactionLoaded(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    error: Option[String]
  ) extends PaymentResult

  case class PayOutTransactionLoaded(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    error: Option[String]
  ) extends PaymentResult

  class PaymentError(override val message: String) extends ErrorMessage(message) with PaymentResult

  case object CardNotPreRegistered extends PaymentError("CardNotPreRegistered")

  case object CardNotPreAuthorized extends PaymentError("CardNotPreAuthorized")

  case class CardPreAuthorizationFailed(resultMessage: String) extends PaymentError(resultMessage)

  case class PayInFailed(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    resultMessage: String
  ) extends PaymentError(resultMessage)

  case class PayOutFailed(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    resultMessage: String
  ) extends PaymentError(resultMessage)

  case class RefundFailed(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    resultMessage: String
  ) extends PaymentError(resultMessage)

  case class TransferFailed(
    transferredTransactionId: String,
    transferredTransactionStatus: Transaction.TransactionStatus,
    resultMessage: String
  ) extends PaymentError(resultMessage)

  case class DirectDebitFailed(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    resultMessage: String
  ) extends PaymentError(resultMessage)

  case object PaymentAccountNotFound extends PaymentError("PaymentAccountNotFound")

  case object MandateAlreadyExists extends PaymentError("MandateAlreadyExists")

  case class MandateCreationFailed(errorCode: String, errorMessage: String)
      extends PaymentError(
        s"MandateCreationFailed: $errorCode -> $errorMessage"
      )

  case object MandateNotCreated extends PaymentError("MandateNotCreated")

  case object MandateNotCanceled extends PaymentError("MandateNotCanceled")

  case object MandateStatusNotUpdated extends PaymentError("MandateStatusNotUpdated")

  case object MandateNotFound extends PaymentError("MandateNotFound")

  case object IllegalMandateStatus extends PaymentError("IllegalMandateStatus")

  case object WrongIban extends PaymentError("WrongIban")

  case object WrongBic extends PaymentError("WrongBic")

  case object WrongSiret extends PaymentError("WrongSiret")

  case object WrongOwnerName extends PaymentError("WrongOwnerName")

  case object WrongOwnerAddress extends PaymentError("WrongOwnerAddress")

  case object UserRequired extends PaymentError("UserRequired")

  case object AcceptedTermsOfPSPRequired extends PaymentError("AcceptedTermsOfPSPRequired")

  case object LegalNameRequired extends PaymentError("LegalNameRequired")

  case object WrongLegalRepresentativeAddress
      extends PaymentError("WrongLegalRepresentativeAddress")

  case object WrongHeadQuartersAddress extends PaymentError("WrongHeadQuartersAddress")

  case object BankAccountNotCreatedOrUpdated extends PaymentError("BankAccountNotCreatedOrUpdated")

  case object KycDocumentNotAdded extends PaymentError("KycDocumentNotAdded")

  case object KycDocumentStatusNotUpdated extends PaymentError("KycDocumentStatusNotUpdated")

  case object KycDocumentStatusNotLoaded extends PaymentError("KycDocumentStatusNotLoaded")

  case object UboNotCreatedOrUpdated extends PaymentError("UboNotCreatedOrUpdated")

  case object UboDeclarationNotAskedForValidation
      extends PaymentError("UboDeclarationNotAskedForValidation")

  case object UboDeclarationNotFound extends PaymentError("UboDeclarationNotFound")

  case object UboDeclarationStatusNotUpdated extends PaymentError("UboDeclarationStatusNotUpdated")

  case object BankAccountNotFound extends PaymentError("BankAccountNotFound")

  case object BankAccountNotDeleted extends PaymentError("BankAccountNotDeleted")

  case object BankAccountDeletionDisabled extends PaymentError("BankAccountDeletionDisabled")

  case object TransactionNotFound extends PaymentError("TransactionNotFound")

  case object IllegalTransactionStatus extends PaymentError("IllegalTransactionStatus")

  case object IllegalTransactionAmount extends PaymentError("IllegalTransactionAmount")

  case object CardsNotLoaded extends PaymentError("CardsNotLoaded")

  case object CardNotDisabled extends PaymentError("CardNotDisabled")

  case object UserNotFound extends PaymentError("UserNotFound")

  case object WalletNotFound extends PaymentError("WalletNotFound")

  case object CardNotFound extends PaymentError("CardNotFound")

  case object RecurringPaymentNotRegistered extends PaymentError("RecurringPaymentNotRegistered")

  case object MandateRequired extends PaymentError("MandateRequired")

  case object RecurringPaymentNotFound extends PaymentError("RecurringPaymentNotFound")

  case object RecurringCardPaymentRegistrationNotUpdated
      extends PaymentError("RecurringCardPaymentRegistrationNotUpdated")

  case class FirstRecurringCardPaymentFailed(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    reason: String
  ) extends PaymentError(s"FirstRecurringPaymentFailed: $reason")

  case class NextRecurringPaymentFailed(
    transactionId: String,
    transactionStatus: Transaction.TransactionStatus,
    reason: String
  ) extends PaymentError(s"NextRecurringPaymentFailed: $reason")

  case object Schedule4PaymentNotTriggered extends PaymentError("Schedule4PaymentNotTriggered")

  case class PayInWithCardPreAuthorizedFailed(error: String) extends PaymentError(error)

  trait ExternalEntityToPaymentEventDecorator extends PaymentEventWithCommand {
    _: ExternalEntityToPaymentEvent =>
    override def command: Option[PaymentCommandEvent] =
      wrapped match {
        case r: ExternalEntityToPaymentEvent.Wrapped.CreateOrUpdatePaymentAccount => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.PayInWithCardPreAuthorized   => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.Refund                       => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.PayOut                       => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.Transfer                     => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.DirectDebit                  => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.LoadDirectDebitTransaction   => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.RegisterRecurringPayment     => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.CancelPreAuthorization       => Some(r.value)
        case r: ExternalEntityToPaymentEvent.Wrapped.CancelMandate                => Some(r.value)
        case _                                                                    => None
      }
  }
}
