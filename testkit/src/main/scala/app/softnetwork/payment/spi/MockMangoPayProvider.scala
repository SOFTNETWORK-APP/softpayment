package app.softnetwork.payment.spi

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.config.MangoPay.MangoPayConfig
import app.softnetwork.payment.config.{MangoPaySettings, Payment, ProviderConfig}
import app.softnetwork.payment.handlers.MockPaymentHandler
import app.softnetwork.payment.model.NaturalUser.NaturalUserType
import app.softnetwork.payment.model.RecurringPayment.RecurringCardPaymentState
import app.softnetwork.payment.model.SoftPayAccount.Client
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.payment.model.{RecurringPayment, _}
import app.softnetwork.payment.service.{
  HooksDirectives,
  HooksEndpoints,
  MangoPayHooksDirectives,
  MangoPayHooksEndpoints
}
import app.softnetwork.persistence._
import app.softnetwork.time.DateExtensions
import com.mangopay.core.enumerations.{TransactionStatus => MangoPayTransactionStatus, _}
import com.mangopay.core.{Address => MangoPayAddress, _}
import com.mangopay.entities.subentities._
import com.mangopay.entities.{
  BankAccount => MangoPayBankAccount,
  Card => _,
  KycDocument => _,
  Transaction => _,
  UboDeclaration => _,
  _
}
import com.typesafe.config.Config
import org.json4s.Formats

import java.text.SimpleDateFormat
import java.util
import java.util.{Calendar, Date, TimeZone}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait MockMangoPayProvider extends MangoPayProvider {

  import MockMangoPayProvider._

  import scala.collection.JavaConverters._

  private val OK = "000000"

  private val SUCCEEDED = "SUCCEEDED"

  private val CREATED = "CREATED"

  /** @param maybeNaturalUser
    *   - natural user to create
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] override def createOrUpdateNaturalUser(
    maybeNaturalUser: Option[NaturalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String]
  ): Option[String] =
    maybeNaturalUser match {
      case Some(naturalUser) =>
        import naturalUser._
        val sdf = new SimpleDateFormat("dd/MM/yyyy")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        Try(sdf.parse(birthday)) match {
          case Success(s) =>
            val user = new UserNatural
            user.setFirstName(firstName)
            user.setLastName(lastName)
            user.setBirthday(s.toEpochSecond)
            user.setEmail(email)
            user.setTag(externalUuid)
            user.setNationality(CountryIso.valueOf(nationality))
            user.setCountryOfResidence(CountryIso.valueOf(countryOfResidence))
            naturalUserType match {
              case Some(value) =>
                value match {
                  case NaturalUserType.PAYER => user.setUserCategory(UserCategory.PAYER)
                  case _ =>
                    user.setUserCategory(UserCategory.OWNER)
                    user.setTermsAndConditionsAccepted(true)
                }
              case _ => // FIXME
            }
            if (userId.getOrElse("").trim.isEmpty) {
              Users.values.find(_.getTag == externalUuid) match {
                case Some(u) => user.setId(u.getId)
                case _       => user.setId(generateUUID())
              }
            } else {
              user.setId(userId.get)
            }
            Users = Users.updated(user.getId, user)
            Some(user.getId)
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }

  /** @param maybeLegalUser
    *   - legal user to create
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] override def createOrUpdateLegalUser(
    maybeLegalUser: Option[LegalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String]
  ): Option[String] = {
    maybeLegalUser match {
      case Some(legalUser) =>
        import legalUser._
        val sdf = new SimpleDateFormat("dd/MM/yyyy")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        Try(sdf.parse(legalRepresentative.birthday)) match {
          case Success(s) =>
            val user = new UserLegal
            user.setId(legalRepresentative.userId.getOrElse(""))
            val headquarters = new MangoPayAddress
            headquarters.setAddressLine1(headQuartersAddress.addressLine)
            headquarters.setCity(headQuartersAddress.city)
            headquarters.setCountry(CountryIso.valueOf(headQuartersAddress.country))
            headquarters.setPostalCode(headQuartersAddress.postalCode)
            user.setHeadquartersAddress(headquarters)
            user.setLegalPersonType(legalUserType)
            user.setName(legalName)
            val c = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
            c.setTime(s)
            val address = new MangoPayAddress
            address.setAddressLine1(legalRepresentativeAddress.addressLine)
            address.setCity(legalRepresentativeAddress.city)
            address.setCountry(CountryIso.valueOf(legalRepresentativeAddress.country))
            address.setPostalCode(legalRepresentativeAddress.postalCode)
            user.setLegalRepresentativeAddress(address)
            user.setLegalRepresentativeBirthday(c.getTimeInMillis / 1000)
            user.setLegalRepresentativeCountryOfResidence(
              CountryIso.valueOf(legalRepresentative.countryOfResidence)
            )
            user.setLegalRepresentativeFirstName(legalRepresentative.firstName)
            user.setLegalRepresentativeLastName(legalRepresentative.lastName)
            user.setLegalRepresentativeNationality(
              CountryIso.valueOf(legalRepresentative.nationality)
            )
            user.setEmail(legalRepresentative.email)
            user.setCompanyNumber(siret)
            legalRepresentative.naturalUserType match {
              case Some(value) =>
                value match {
                  case NaturalUserType.PAYER => user.setUserCategory(UserCategory.PAYER)
                  case _ =>
                    user.setUserCategory(UserCategory.OWNER)
                    user.setTermsAndConditionsAccepted(true)
                }
              case _ =>
                user.setUserCategory(UserCategory.OWNER)
                user.setTermsAndConditionsAccepted(true)
            }
            user.setTag(legalRepresentative.externalUuid)
            if (legalRepresentative.userId.isEmpty) {
              LegalUsers.values.find(_.getTag == legalRepresentative.externalUuid) match {
                case Some(u) =>
                  user.setId(u.getId)
                  LegalUsers = LegalUsers.updated(user.getId, user)
                  Some(user.getId)
                case _ =>
                  user.setId(generateUUID())
                  LegalUsers = LegalUsers.updated(user.getId, user)
                  Some(user.getId)
              }
            } else {
              user.setId(legalRepresentative.userId.get)
              LegalUsers = LegalUsers.updated(user.getId, user)
              Some(user.getId)
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
  }

  /** @param maybePayOutTransaction
    *   - pay out transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay out transaction result
    */
  override def payOut(
    maybePayOutTransaction: Option[PayOutTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] =
    maybePayOutTransaction match {
      case Some(payOutTransaction) =>
        import payOutTransaction._
        val payOut = new PayOut
        payOut.setTag(orderUuid)
        payOut.setAuthorId(authorId)
        payOut.setCreditedUserId(creditedUserId)
        payOut.setDebitedFunds(new Money)
        payOut.getDebitedFunds.setAmount(debitedAmount)
        payOut.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        payOut.setFees(new Money)
        payOut.getFees.setAmount(feesAmount)
        payOut.getFees.setCurrency(CurrencyIso.valueOf(currency))
        payOut.setDebitedWalletId(debitedWalletId)
        val meanOfPaymentDetails = new PayOutPaymentDetailsBankWire
        meanOfPaymentDetails.setBankAccountId(bankAccountId)
        payOut.setMeanOfPaymentDetails(meanOfPaymentDetails)
        payOut.setId(generateUUID())
        mlog.info(s"debitedAmount -> $debitedAmount, fees -> $feesAmount")
        assert(debitedAmount > feesAmount)
        payOut.setStatus(MangoPayTransactionStatus.SUCCEEDED)
        payOut.setResultCode(OK)
        payOut.setResultMessage(SUCCEEDED)
        PayOuts = PayOuts.updated(payOut.getId, payOut)
        Some(
          Transaction()
            .copy(
              id = payOut.getId,
              orderUuid = orderUuid,
              nature = Transaction.TransactionNature.REGULAR,
              `type` = Transaction.TransactionType.PAYOUT,
              status = payOut.getStatus,
              amount = debitedAmount,
              fees = feesAmount,
              resultCode = payOut.getResultCode,
              resultMessage = payOut.getResultMessage,
              authorId = authorId,
              creditedUserId = Some(creditedUserId),
              debitedWalletId = Some(debitedWalletId)
            )
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
        )
      case _ => None
    }

  /** @param maybeBankAccount
    *   - bank account to create
    * @return
    *   bank account id
    */
  override def createOrUpdateBankAccount(maybeBankAccount: Option[BankAccount]): Option[String] =
    maybeBankAccount match {
      case Some(mangoPayBankAccount) =>
        import mangoPayBankAccount._
        val bankAccount = new MangoPayBankAccount
        bankAccount.setActive(true)
        val details = new BankAccountDetailsIBAN
        details.setIban(iban)
        if (bic.trim.nonEmpty) {
          details.setBic(bic)
        }
        bankAccount.setDetails(details)
        bankAccount.setOwnerName(ownerName)
        val address = new MangoPayAddress
        address.setAddressLine1(ownerAddress.addressLine)
        address.setCity(ownerAddress.city)
        address.setCountry(CountryIso.valueOf(ownerAddress.country))
        address.setPostalCode(ownerAddress.postalCode)
        bankAccount.setOwnerAddress(address)
        bankAccount.setTag(tag)
        bankAccount.setType(BankAccountType.IBAN)
        bankAccount.setUserId(userId)
        BankAccounts.values.find(bankAccount =>
          bankAccount.isActive && bankAccount.getId == id.getOrElse("")
        ) match {
          case Some(ba) if checkEquality(bankAccount, ba) =>
            bankAccount.setId(ba.getId)
            BankAccounts = BankAccounts.updated(bankAccount.getId, bankAccount)
            Some(bankAccount.getId)
          case _ =>
            bankAccount.setId(generateUUID())
            BankAccounts = BankAccounts.updated(bankAccount.getId, bankAccount)
            Some(bankAccount.getId)
        }
      case _ => None
    }

  /** @param maybeRefundTransaction
    *   - refund transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   refund transaction result
    */
  override def refund(
    maybeRefundTransaction: Option[RefundTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] =
    maybeRefundTransaction match {
      case Some(refundTransaction) =>
        import refundTransaction._
        val refund = new Refund
        refund.setTag(orderUuid)
        refund.setInitialTransactionType(InitialTransactionType.PAYIN)
        refund.setInitialTransactionId(payInTransactionId)
        refund.setAuthorId(authorId)
        refund.setRefundReason(new RefundReason)
        refund.getRefundReason.setRefundReasonMessage(reasonMessage)
        if (initializedByClient) {
          refund.getRefundReason.setRefundReasonType(RefundReasonType.INITIALIZED_BY_CLIENT)
        } else {
          refund.getRefundReason.setRefundReasonType(RefundReasonType.OTHER)
        }
        refund.setDebitedFunds(new Money)
        refund.getDebitedFunds.setAmount(refundAmount)
        refund.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        refund.setFees(new Money)
        refund.getFees.setAmount(0) // fees are only set during transfer or payOut
        refund.getFees.setCurrency(CurrencyIso.valueOf(currency))

        refund.setId(generateUUID())
        refund.setStatus(MangoPayTransactionStatus.SUCCEEDED)
        refund.setResultCode(OK)
        refund.setResultMessage(SUCCEEDED)
        Refunds = Refunds.updated(refund.getId, refund)

        Some(
          Transaction().copy(
            id = refund.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REFUND,
            `type` = Transaction.TransactionType.PAYIN,
            status = refund.getStatus,
            amount = refundAmount,
            fees = 0,
            resultCode = refund.getResultCode,
            resultMessage = refund.getResultMessage,
            reasonMessage = Option(reasonMessage),
            authorId = authorId
          )
        )
      case _ => None
    }

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
        import transferTransaction._
        val transfer = new Transfer
        transfer.setAuthorId(authorId)
        transfer.setCreditedUserId(creditedUserId)
        transfer.setCreditedWalletId(creditedWalletId)
        transfer.setDebitedFunds(new Money)
        transfer.getDebitedFunds.setAmount(debitedAmount)
        transfer.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        transfer.setFees(new Money)
        transfer.getFees.setAmount(feesAmount)
        transfer.getFees.setCurrency(CurrencyIso.valueOf(currency))
        transfer.setDebitedWalletId(debitedWalletId)
        transfer.setId(generateUUID())

        transfer.setStatus(MangoPayTransactionStatus.SUCCEEDED)
        transfer.setResultCode(OK)
        transfer.setResultMessage(SUCCEEDED)
        Transfers = Transfers.updated(transfer.getId, transfer)
        Some(
          Transaction()
            .copy(
              id = transfer.getId,
              nature = Transaction.TransactionNature.REGULAR,
              `type` = Transaction.TransactionType.TRANSFER,
              status = transfer.getStatus,
              amount = debitedAmount,
              fees = feesAmount,
              resultCode = transfer.getResultCode,
              resultMessage = transfer.getResultMessage,
              authorId = authorId,
              creditedUserId = Some(creditedUserId),
              creditedWalletId = Some(creditedWalletId),
              debitedWalletId = Some(debitedWalletId),
              orderUuid = orderUuid.getOrElse(""),
              externalReference = externalReference
            )
            .withPaymentType(Transaction.PaymentType.BANK_WIRE)
        )
      case _ => None
    }
  }

  /** @param cardId
    *   - card id
    * @return
    *   card
    */
  override def loadCard(cardId: String): Option[Card] =
    Cards.get(cardId) match {
      case None =>
        CardRegistrations.values.find(_.getCardId == cardId) match {
          case Some(_) =>
            Cards = Cards.updated(
              cardId,
              Card.defaultInstance
                .withId(cardId)
                .withAlias("##################")
                .withExpirationDate(new SimpleDateFormat("MMyy").format(now()))
                .withActive(true)
            )
            Cards.get(cardId)
          case _ => None
        }
      case some => some
    }

  /** @param cardId
    *   - the id of the card to disable
    * @return
    *   the card disabled or none
    */
  override def disableCard(cardId: String): Option[Card] = {
    Cards.get(cardId) match {
      case Some(card) =>
        Cards = Cards.updated(card.id, card.withActive(false))
        Cards.get(card.id)
      case _ => None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay in transaction
    */
  override def loadPayInTransaction(
    orderUuid: String,
    transactionId: String,
    recurringPayInRegistrationId: Option[String]
  ): Option[Transaction] =
    PayIns.get(transactionId) match {
      case Some(result) =>
        val `type` =
          if (result.getPaymentType == PayInPaymentType.DIRECT_DEBIT) {
            Transaction.TransactionType.DIRECT_DEBIT
          } else if (result.getPaymentType == PayInPaymentType.PREAUTHORIZED) {
            Transaction.TransactionType.PRE_AUTHORIZATION
          } else {
            Transaction.TransactionType.PAYIN
          }
        val cardId =
          if (result.getPaymentType == PayInPaymentType.CARD) {
            Option(result.getPaymentDetails.asInstanceOf[PayInPaymentDetailsCard].getCardId)
          } else {
            None
          }
        val redirectUrl =
          if (result.getExecutionType == PayInExecutionType.DIRECT) {
            Option( // for 3D Secure
              result.getExecutionDetails
                .asInstanceOf[PayInExecutionDetailsDirect]
                .getSecureModeRedirectUrl
            )
          } else if (result.getExecutionType == PayInExecutionType.WEB) {
            Option(
              result.getExecutionDetails
                .asInstanceOf[PayInExecutionDetailsWeb]
                .getRedirectUrl
            )
          } else {
            None
          }
        val returnUrl =
          if (result.getExecutionType == PayInExecutionType.WEB) {
            Option(
              result.getExecutionDetails
                .asInstanceOf[PayInExecutionDetailsWeb]
                .getReturnUrl
            )
          } else {
            None
          }
        val mandateId =
          if (result.getPaymentType == PayInPaymentType.DIRECT_DEBIT) {
            Option(
              result.getPaymentDetails.asInstanceOf[PayInPaymentDetailsDirectDebit].getMandateId
            )
          } else {
            None
          }
        val preAuthorizationId =
          if (result.getPaymentType == PayInPaymentType.PREAUTHORIZED) {
            Option(
              result.getExecutionDetails
                .asInstanceOf[PayInPaymentDetailsPreAuthorized]
                .getPreauthorizationId
            )
          } else {
            None
          }
        val status: Transaction.TransactionStatus = result.getStatus
        val payPalBuyerAccountEmail =
          if (result.getPaymentType == PayInPaymentType.PAYPAL) {
            Option(result.getPaymentDetails).flatMap(details =>
              Option(details.asInstanceOf[PayInPaymentDetailsPayPal].getPaypalBuyerAccountEmail)
            )
          } else {
            None
          }
        Some(
          Transaction().copy(
            id = transactionId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = `type`,
            status =
              if (status.isTransactionCreated && redirectUrl.isDefined)
                Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
              else status,
            amount = result.getDebitedFunds.getAmount,
            cardId = cardId,
            fees = result.getFees.getAmount,
            resultCode = result.getResultCode,
            resultMessage = result.getResultMessage,
            redirectUrl = redirectUrl,
            authorId = result.getAuthorId,
            creditedUserId = Option(result.getCreditedUserId),
            creditedWalletId = Option(result.getCreditedWalletId),
            mandateId = mandateId,
            preAuthorizationId = preAuthorizationId,
            recurringPayInRegistrationId = recurringPayInRegistrationId,
            paymentType = result.getPaymentType,
            returnUrl = returnUrl,
            payPalBuyerAccountEmail = payPalBuyerAccountEmail
          )
        )
      case _ => None
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
  ): Option[Transaction] = None

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
  ): Option[Transaction] = None

  /** @param transactionId
    *   - transaction id
    * @return
    *   transfer transaction
    */
  override def loadTransfer(transactionId: String): Option[Transaction] = None

  /** @param cardPreRegistrationId
    *   - card registration id
    * @param maybeRegistrationData
    *   - card registration data
    * @return
    *   card id
    */
  override def createCard(
    cardPreRegistrationId: String,
    maybeRegistrationData: Option[String]
  ): Option[String] =
    maybeRegistrationData match {
      case Some(_) =>
        CardRegistrations.get(cardPreRegistrationId) match {
          case Some(cr) /* FIXME if cr.RegistrationData == registrationData*/ =>
            cr.setCardId(generateUUID())
            CardRegistrations = CardRegistrations.updated(cardPreRegistrationId, cr)
            Some(cr.getCardId)
          case _ => None
        }
      case _ => None
    }

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
  override def createOrUpdateWallet(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String,
    maybeWalletId: Option[String]
  ): Option[String] =
    maybeUserId match {
      case Some(userId) =>
        val wallet = new Wallet
        wallet.setCurrency(CurrencyIso.valueOf(currency))
        wallet.setOwners(new util.ArrayList(List(userId).asJava))
        wallet.setDescription(s"wallet for $externalUuid")
        wallet.setTag(externalUuid)
        maybeWalletId match {
          case Some(walletId) =>
            wallet.setId(walletId)
            Wallets = Wallets.updated(wallet.getId, wallet)
            Some(wallet.getId)
          case _ =>
            Wallets.values.find(_.getTag == externalUuid) match {
              case Some(w) =>
                wallet.setId(w.getId)
                Wallets = Wallets.updated(wallet.getId, wallet)
                Some(wallet.getId)
              case _ =>
                wallet.setId(generateUUID())
                Wallets = Wallets.updated(wallet.getId, wallet)
                Some(wallet.getId)
            }
        }
      case _ => None
    }

  /** @param userId
    *   - provider user id
    * @return
    *   the first active bank account
    */
  override def getActiveBankAccount(userId: String, currency: String): Option[String] =
    BankAccounts.values.filter(bankAccount =>
      bankAccount.getUserId == userId && bankAccount.isActive
    ) match {
      case bas if bas.nonEmpty =>
        Some(bas.toList.sortWith(_.getCreationDate > _.getCreationDate).head.getId)
      case _ => None
    }

  /** @param preAuthorizationTransaction
    *   - pre authorization transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   re authorization transaction result
    */
  override def preAuthorizeCard(
    preAuthorizationTransaction: PreAuthorizationTransaction,
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    import preAuthorizationTransaction._
    val cardPreAuthorization = new CardPreAuthorization()
    cardPreAuthorization.setTag(orderUuid)
    cardPreAuthorization.setAuthorId(authorId)
    cardPreAuthorization.setCardId(cardId)
    cardPreAuthorization.setDebitedFunds(new Money)
    cardPreAuthorization.getDebitedFunds.setAmount(debitedAmount)
    cardPreAuthorization.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
    cardPreAuthorization.setRemainingFunds(cardPreAuthorization.getDebitedFunds)
    cardPreAuthorization.setExecutionType(PreAuthorizationExecutionType.DIRECT)
    cardPreAuthorization.setSecureMode(SecureMode.DEFAULT)
    cardPreAuthorization.setSecureModeReturnUrl(
      s"${config.preAuthorizeCardReturnUrl}/$orderUuid?registerCard=${registerCard
        .getOrElse(false)}&printReceipt=${printReceipt.getOrElse(false)}"
    )

    cardPreAuthorization.setId(generateUUID())
    cardPreAuthorization.setStatus(PreAuthorizationStatus.CREATED)
    cardPreAuthorization.setResultCode(OK)
    cardPreAuthorization.setResultMessage(CREATED)
    cardPreAuthorization.setSecureModeRedirectUrl(
      s"${cardPreAuthorization.getSecureModeReturnUrl}&preAuthorizationId=${cardPreAuthorization.getId}"
    )
    cardPreAuthorization.setPaymentStatus(PaymentStatus.WAITING)
    CardPreAuthorizations =
      CardPreAuthorizations.updated(cardPreAuthorization.getId, cardPreAuthorization)

    Some(
      Transaction().copy(
        id = cardPreAuthorization.getId,
        orderUuid = orderUuid,
        nature = Transaction.TransactionNature.REGULAR,
        `type` = Transaction.TransactionType.PRE_AUTHORIZATION,
        status = cardPreAuthorization.getStatus,
        amount = debitedAmount,
        cardId = Option(cardId),
        fees = 0,
        resultCode = cardPreAuthorization.getResultCode,
        resultMessage = cardPreAuthorization.getResultMessage,
        redirectUrl =
          if (debitedAmount > 5000) Option(cardPreAuthorization.getSecureModeRedirectUrl)
          else None,
        authorId = cardPreAuthorization.getAuthorId,
        paymentType = Transaction.PaymentType.CARD
      )
    )
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   card pre authorized transaction
    */
  override def loadCardPreAuthorized(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Option[Transaction] = {
    CardPreAuthorizations.get(cardPreAuthorizedTransactionId) match {
      case Some(result) =>
        Some(
          Transaction().copy(
            id = result.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PRE_AUTHORIZATION,
            status = result.getStatus,
            amount = result.getDebitedFunds.getAmount,
            cardId = Option(result.getCardId),
            fees = 0,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
            redirectUrl = None /*Option( // for 3D Secure
              result.getSecureModeRedirectUrl
            )*/,
            authorId = result.getAuthorId,
            preAuthorizationCanceled = Option(result.getPaymentStatus == PaymentStatus.CANCELED),
            preAuthorizationValidated = Option(result.getPaymentStatus == PaymentStatus.VALIDATED),
            preAuthorizationExpired = Option(result.getPaymentStatus == PaymentStatus.EXPIRED)
          )
        )
      case _ => None
    }
  }

  /** @param payInWithCardPreAuthorizedTransaction
    *   - card pre authorized pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with card pre authorized transaction result
    */
  private[spi] override def payInWithCardPreAuthorized(
    payInWithCardPreAuthorizedTransaction: Option[PayInWithCardPreAuthorizedTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    payInWithCardPreAuthorizedTransaction match {
      case Some(payInWithCardPreAuthorizedTransaction) =>
        import payInWithCardPreAuthorizedTransaction._
        val payIn = new PayIn()
        payIn.setTag(orderUuid)
        payIn.setCreditedWalletId(creditedWalletId)
        payIn.setAuthorId(authorId)
        payIn.setDebitedFunds(new Money)
        payIn.getDebitedFunds.setAmount(debitedAmount)
        payIn.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setExecutionType(PayInExecutionType.DIRECT)
        payIn.setFees(new Money)
        payIn.getFees.setAmount(0) // fees are only set during transfer or payOut
        payIn.getFees.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setPaymentType(PayInPaymentType.PREAUTHORIZED)
        val paymentDetails = new PayInPaymentDetailsPreAuthorized
        paymentDetails.setPreauthorizationId(cardPreAuthorizedTransactionId)
        payIn.setPaymentDetails(paymentDetails)

        payIn.setId(generateUUID())
        payIn.setStatus(MangoPayTransactionStatus.SUCCEEDED)
        payIn.setResultCode(OK)
        payIn.setResultMessage(SUCCEEDED)
        PayIns = PayIns.updated(payIn.getId, payIn)
        Some(
          Transaction()
            .copy(
              id = payIn.getId,
              orderUuid = orderUuid,
              nature = Transaction.TransactionNature.REGULAR,
              `type` = Transaction.TransactionType.PAYIN,
              status = payIn.getStatus,
              amount = debitedAmount,
              cardId = None,
              fees = 0,
              resultCode = Option(payIn.getResultCode).getOrElse(""),
              resultMessage = Option(payIn.getResultMessage).getOrElse(""),
              redirectUrl = None,
              authorId = payIn.getAuthorId,
              creditedWalletId = Option(payIn.getCreditedWalletId)
            )
            .withPaymentType(Transaction.PaymentType.PREAUTHORIZED)
            .withPreAuthorizationId(cardPreAuthorizedTransactionId)
            .withPreAuthorizationDebitedAmount(preAuthorizationDebitedAmount)
        )
      case _ => None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been cancelled or not
    */
  override def cancelPreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Boolean = {
    CardPreAuthorizations.get(cardPreAuthorizedTransactionId) match {
      case Some(result) =>
        result.setPaymentStatus(PaymentStatus.CANCELED)
        CardPreAuthorizations = CardPreAuthorizations.updated(result.getId, result)
        true
      case _ => false
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been cancelled or not
    */
  override def validatePreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Boolean = {
    CardPreAuthorizations.get(cardPreAuthorizedTransactionId) match {
      case Some(result) if result.getPaymentStatus == PaymentStatus.WAITING =>
        result.setPaymentStatus(PaymentStatus.VALIDATED)
        CardPreAuthorizations = CardPreAuthorizations.updated(result.getId, result)
        true
      case _ => false
    }
  }

  /** @param maybePayInTransaction
    *   - pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in transaction result
    */
  private[spi] override def payInWithCard(
    maybePayInTransaction: Option[PayInWithCardTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] =
    maybePayInTransaction match {
      case Some(payInTransaction) =>
        import payInTransaction._
        val payIn = new PayIn()
        payIn.setTag(orderUuid)
        payIn.setCreditedWalletId(creditedWalletId)
        payIn.setAuthorId(authorId)
        payIn.setDebitedFunds(new Money)
        payIn.getDebitedFunds.setAmount(debitedAmount)
        payIn.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setFees(new Money)
        payIn.getFees.setAmount(0) // fees are only set during transfer or payOut
        payIn.getFees.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setPaymentType(PayInPaymentType.CARD)
        val paymentDetails = new PayInPaymentDetailsCard
        paymentDetails.setCardId(cardId)
        paymentDetails.setCardType(CardType.CB_VISA_MASTERCARD)
        paymentDetails.setStatementDescriptor(statementDescriptor)
        payIn.setPaymentDetails(paymentDetails)
        payIn.setExecutionType(PayInExecutionType.DIRECT)
        val executionDetails = new PayInExecutionDetailsDirect
        executionDetails.setCardId(cardId)
        // Secured Mode is activated from €100.
        executionDetails.setSecureMode(SecureMode.DEFAULT)
        executionDetails.setSecureModeReturnUrl(
          s"${config.payInReturnUrl}/$orderUuid?registerCard=${registerCard
            .getOrElse(false)}&printReceipt=${printReceipt.getOrElse(false)}"
        )
        payIn.setExecutionDetails(executionDetails)

        payIn.setId(generateUUID())
        payIn.setStatus(MangoPayTransactionStatus.CREATED)
        payIn.setResultCode(OK)
        payIn.setResultMessage(CREATED)
        executionDetails.setSecureModeRedirectUrl(
          s"${executionDetails.getSecureModeReturnUrl}&transactionId=${payIn.getId}"
        )
        PayIns = PayIns.updated(payIn.getId, payIn)

        Some(
          Transaction().copy(
            id = payIn.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PAYIN,
            status = payIn.getStatus,
            amount = debitedAmount,
            cardId = Option(cardId),
            fees = 0,
            resultCode = payIn.getResultCode,
            resultMessage = payIn.getResultMessage,
            redirectUrl =
              if (debitedAmount > 5000) Option(executionDetails.getSecureModeRedirectUrl) else None,
            authorId = authorId,
            creditedWalletId = Some(creditedWalletId)
          )
        )
      case _ => None
    }

  /** @param payInWithPayPalTransaction
    *   - pay in with PayPal transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in with PayPal transaction result
    */
  private[spi] override def payInWithPayPal(
    payInWithPayPalTransaction: Option[PayInWithPayPalTransaction],
    idempotency: Option[Boolean]
  ): Option[Transaction] = {
    payInWithPayPalTransaction match {
      case Some(payInWithPayPalTransaction) =>
        import payInWithPayPalTransaction._
        val payIn = new PayIn()
        payIn.setTag(orderUuid)
        payIn.setCreditedWalletId(creditedWalletId)
        payIn.setAuthorId(authorId)
        payIn.setDebitedFunds(new Money)
        payIn.getDebitedFunds.setAmount(debitedAmount)
        payIn.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setFees(new Money)
        payIn.getFees.setAmount(0) // fees are only set during transfer or payOut
        payIn.getFees.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setPaymentType(PayInPaymentType.PAYPAL)
        val executionDetails = new PayInExecutionDetailsWeb()
        executionDetails.setCulture(language)
        executionDetails.setReturnUrl(
          s"${config.payInReturnUrl}/$orderUuid?printReceipt=${printReceipt.getOrElse(false)}"
        )
        payIn.setExecutionDetails(executionDetails)
        payIn.setExecutionType(PayInExecutionType.WEB)

        payIn.setId(generateUUID())
        payIn.setStatus(MangoPayTransactionStatus.CREATED)
        payIn.setResultCode(OK)
        payIn.setResultMessage(CREATED)
        executionDetails.setRedirectUrl(
          s"${executionDetails.getReturnUrl}&transactionId=${payIn.getId}"
        )
        PayIns = PayIns.updated(payIn.getId, payIn)

        Some(
          Transaction().copy(
            id = payIn.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PAYIN,
            status = payIn.getStatus,
            amount = debitedAmount,
            fees = 0,
            resultCode = Option(payIn.getResultCode).getOrElse(""),
            resultMessage = Option(payIn.getResultMessage).getOrElse(""),
            redirectUrl = Option(
              payIn.getExecutionDetails
                .asInstanceOf[PayInExecutionDetailsWeb]
                .getRedirectUrl
            ),
            returnUrl = Option(
              payIn.getExecutionDetails
                .asInstanceOf[PayInExecutionDetailsWeb]
                .getReturnUrl
            ),
            authorId = payIn.getAuthorId,
            creditedWalletId = Option(payIn.getCreditedWalletId),
            paymentType = Transaction.PaymentType.PAYPAL
          )
        )
      case None => None
    }
  }

  /** @param maybeUserId
    *   - owner of the card
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @return
    *   card pre registration
    */
  override def preRegisterCard(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String
  ): Option[CardPreRegistration] =
    maybeUserId match {
      case Some(userId) =>
        val cardPreRegistration = new CardRegistration()
        cardPreRegistration.setCurrency(CurrencyIso.valueOf(currency))
        cardPreRegistration.setTag(externalUuid)
        cardPreRegistration.setUserId(userId)
        cardPreRegistration.setId(generateUUID())
        cardPreRegistration.setAccessKey("key")
        cardPreRegistration.setPreregistrationData("data")
        cardPreRegistration.setCardRegistrationUrl("url")
        CardRegistrations =
          CardRegistrations.updated(cardPreRegistration.getId, cardPreRegistration)
        Some(
          CardPreRegistration.defaultInstance
            .withId(cardPreRegistration.getId)
            .withAccessKey(cardPreRegistration.getAccessKey)
            .withPreregistrationData(cardPreRegistration.getPreregistrationData)
            .withCardRegistrationURL(cardPreRegistration.getCardRegistrationUrl)
        )
      case _ => None
    }

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
  override def addDocument(
    userId: String,
    externalUuid: String,
    pages: Seq[Array[Byte]],
    documentType: KycDocument.KycDocumentType
  ): Option[String] = {
    val documentId = generateUUID()
    Documents = Documents.updated(
      documentId,
      KycDocumentValidationReport.defaultInstance
        .withId(documentId)
        .withType(documentType)
        .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED)
    )
    Some(documentId)
  }

  /** @param userId
    *   - Provider user id
    * @param documentId
    *   - Provider document id
    * @return
    *   document validation report
    */
  override def loadDocumentStatus(
    userId: String,
    documentId: String,
    documentType: KycDocument.KycDocumentType
  ): KycDocumentValidationReport =
    Documents.getOrElse(
      documentId,
      KycDocumentValidationReport.defaultInstance
        .withId(documentId)
        .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED)
    )

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
  override def mandate(
    externalUuid: String,
    userId: String,
    bankAccountId: String,
    idempotencyKey: Option[String] = None
  ): Option[MandateResult] = {
    val mandate = new Mandate()
    mandate.setId(generateUUID())
    mandate.setBankAccountId(bankAccountId)
    mandate.setCulture(CultureCode.FR)
    mandate.setExecutionType(MandateExecutionType.WEB)
    mandate.setMandateType(MandateType.DIRECT_DEBIT)
    mandate.setReturnUrl(
      s"${config.mandateReturnUrl}?externalUuid=$externalUuid&idempotencyKey=${idempotencyKey.getOrElse("")}"
    )
    mandate.setScheme(MandateScheme.SEPA)
    mandate.setStatus(MandateStatus.SUBMITTED)
    mandate.setUserId(userId)
    Mandates = Mandates.updated(mandate.getId, mandate)
    Some(
      MandateResult.defaultInstance.withId(mandate.getId).withStatus(mandate.getStatus)
    )
  }

  /** @param maybeMandateId
    *   - optional mandate id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - bank account id
    * @return
    *   mandate associated to this bank account
    */
  override def loadMandate(
    maybeMandateId: Option[String],
    userId: String,
    bankAccountId: String
  ): Option[MandateResult] = {
    maybeMandateId match {
      case Some(mandateId) =>
        Mandates.get(mandateId) match {
          case Some(s) if s.getBankAccountId == bankAccountId && s.getUserId == userId =>
            Some(
              MandateResult.defaultInstance.withId(s.getId).withStatus(s.getStatus)
            )
          case _ => None
        }
      case _ =>
        Mandates.values
          .filter(m => m.getBankAccountId == bankAccountId && m.getUserId == userId)
          .map(m => MandateResult.defaultInstance.withId(m.getId).withStatus(m.getStatus))
          .headOption
    }
  }

  /** @param mandateId
    *   - Provider mandate id
    * @return
    *   mandate result
    */
  override def cancelMandate(mandateId: String): Option[MandateResult] = {
    Mandates.get(mandateId) match {
      case Some(mandate) =>
        Mandates = Mandates.updated(mandateId, null)
        Some(
          MandateResult.defaultInstance.withId(mandate.getId).withStatus(mandate.getStatus)
        )
      case _ => None
    }
  }

  /** @param maybeDirectDebitTransaction
    *   - direct debit transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   direct debit transaction result
    */
  override def directDebit(
    maybeDirectDebitTransaction: Option[DirectDebitTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] = {
    maybeDirectDebitTransaction match {
      case Some(directDebitTransaction) =>
        import directDebitTransaction._
        val payIn = new PayIn()
        payIn.setAuthorId(authorId)
        payIn.setCreditedUserId(creditedUserId)
        payIn.setCreditedWalletId(creditedWalletId)
        payIn.setDebitedFunds(new Money)
        payIn.getDebitedFunds.setAmount(debitedAmount)
        payIn.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setFees(new Money)
        payIn.getFees.setAmount(feesAmount)
        payIn.getFees.setCurrency(CurrencyIso.valueOf(currency))
        payIn.setPaymentType(PayInPaymentType.DIRECT_DEBIT)
        val paymentDetails = new PayInPaymentDetailsDirectDebit
        paymentDetails.setCulture(CultureCode.FR)
        paymentDetails.setMandateId(mandateId)
        paymentDetails.setStatementDescriptor(statementDescriptor)
        payIn.setPaymentDetails(paymentDetails)
        payIn.setExecutionType(PayInExecutionType.DIRECT)
        val executionDetails = new PayInExecutionDetailsDirect
        executionDetails.setCulture(CultureCode.FR)
        // Secured Mode is activated from €100.
        executionDetails.setSecureMode(SecureMode.DEFAULT)
        payIn.setExecutionDetails(executionDetails)

        payIn.setId(generateUUID())
        payIn.setStatus(MangoPayTransactionStatus.SUCCEEDED)
        payIn.setResultCode(OK)
        payIn.setResultMessage(SUCCEEDED)
        PayIns = PayIns.updated(payIn.getId, payIn)
        ClientFees += feesAmount.toDouble / 100
        Some(
          Transaction()
            .copy(
              id = payIn.getId,
              nature = Transaction.TransactionNature.REGULAR,
              `type` = Transaction.TransactionType.DIRECT_DEBIT,
              status = payIn.getStatus,
              amount = debitedAmount,
              fees = feesAmount,
              resultCode = payIn.getResultCode,
              resultMessage = payIn.getResultMessage,
              redirectUrl = None,
              authorId = authorId,
              creditedUserId = Some(creditedUserId),
              creditedWalletId = Some(creditedWalletId),
              mandateId = Some(mandateId),
              externalReference = externalReference
            )
            .withPaymentType(Transaction.PaymentType.DIRECT_DEBITED)
        )
      case _ => None
    }
  }

  /** @param walletId
    *   - Provider wallet id
    * @param transactionId
    *   - Provider transaction id
    * @param transactionDate
    *   - Provider transaction date
    * @return
    *   transaction if it exists
    */
  override def loadDirectDebitTransaction(
    walletId: String,
    transactionId: String,
    transactionDate: Date
  ): Option[Transaction] = {
    PayIns.get(transactionId) match {
      case Some(payIn) =>
        Some(
          Transaction().copy(
            id = payIn.getId,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PAYIN,
            status = payIn.getStatus,
            amount = payIn.getDebitedFunds.getAmount,
            fees = payIn.getFees.getAmount,
            resultCode = payIn.getResultCode,
            resultMessage = payIn.getResultMessage,
            redirectUrl = None,
            authorId = payIn.getAuthorId,
            creditedUserId = Option(payIn.getCreditedUserId),
            creditedWalletId = Option(payIn.getCreditedWalletId)
          )
        )
      case _ => None
    }
  }

  override def client: Option[SoftPayAccount.Client] =
    Some(
      SoftPayAccount.Client.defaultInstance
        .withProvider(provider)
        .withClientId(provider.clientId)
    )

  /** @return
    *   client fees
    */
  override def clientFees(): Option[Double] = Some(ClientFees)

  /** @param userId
    *   - Provider user id
    * @return
    *   Ultimate Beneficial Owner Declaration
    */
  override def createDeclaration(userId: String): Option[UboDeclaration] = {
    val uboDeclaration = UboDeclaration.defaultInstance
      .withId(generateUUID())
      .withStatus(UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_CREATED)
      .withCreatedDate(now())
    UboDeclarations = UboDeclarations.updated(uboDeclaration.id, uboDeclaration)
    Some(uboDeclaration)
  }

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @param ultimateBeneficialOwner
    *   - Ultimate Beneficial Owner
    * @return
    *   Ultimate Beneficial Owner created or updated
    */
  override def createOrUpdateUBO(
    userId: String,
    uboDeclarationId: String,
    ultimateBeneficialOwner: UboDeclaration.UltimateBeneficialOwner
  ): Option[UboDeclaration.UltimateBeneficialOwner] = {
    UboDeclarations.get(uboDeclarationId) match {
      case Some(uboDeclaration) =>
        ultimateBeneficialOwner.id match {
          case Some(id) =>
            UboDeclarations = UboDeclarations.updated(
              uboDeclarationId,
              uboDeclaration
                .withUbos(
                  uboDeclaration.ubos.filterNot(_.id.getOrElse("") == id) :+ ultimateBeneficialOwner
                )
            )
            Some(ultimateBeneficialOwner)
          case _ =>
            val updated = ultimateBeneficialOwner.withId(generateUUID())
            UboDeclarations = UboDeclarations.updated(
              uboDeclarationId,
              uboDeclaration.withUbos(uboDeclaration.ubos :+ updated)
            )
            Some(updated)
        }
      case _ => None
    }
  }

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   declaration with Ultimate Beneficial Owner(s)
    */
  override def getDeclaration(userId: String, uboDeclarationId: String): Option[UboDeclaration] =
    UboDeclarations.get(uboDeclarationId)

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   Ultimate Beneficial Owner declaration
    */
  override def validateDeclaration(
    userId: String,
    uboDeclarationId: String,
    ipAddress: String,
    userAgent: String
  ): Option[UboDeclaration] = {
    UboDeclarations.get(uboDeclarationId) match {
      case Some(uboDeclaration) =>
        val updated = uboDeclaration.withStatus(
          UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATION_ASKED
        )
        UboDeclarations = UboDeclarations.updated(
          uboDeclarationId,
          updated
        )
        Some(updated)
      case _ => None
    }
  }

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
  override def registerRecurringCardPayment(
    userId: String,
    walletId: String,
    cardId: String,
    recurringPayment: RecurringPayment
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    if (recurringPayment.`type`.isCard) {
      import recurringPayment.{cardId => _, _}
      val createRecurringPayment = new CreateRecurringPayment
      createRecurringPayment.setAuthorId(userId)
      createRecurringPayment.setCreditedUserId(userId)
      createRecurringPayment.setCreditedWalletId(walletId)
      createRecurringPayment.setCardId(cardId)
      val firstTransactionDebitedFunds = new Money
      firstTransactionDebitedFunds.setAmount(firstDebitedAmount)
      firstTransactionDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
      createRecurringPayment.setFirstTransactionDebitedFunds(firstTransactionDebitedFunds)
      val firstTransactionFees = new Money
      firstTransactionFees.setAmount(firstFeesAmount)
      firstTransactionFees.setCurrency(CurrencyIso.valueOf(currency))
      createRecurringPayment.setFirstTransactionFees(firstTransactionFees)
      recurringPayment.endDate match {
        case Some(endDate) =>
          createRecurringPayment.setEndDate(endDate.toEpochSecond)
        case _ =>
      }
      (recurringPayment.frequency match {
        case Some(frequency) =>
          frequency match {
            case RecurringPayment.RecurringPaymentFrequency.DAILY     => Some("Daily")
            case RecurringPayment.RecurringPaymentFrequency.WEEKLY    => Some("Weekly")
            case RecurringPayment.RecurringPaymentFrequency.MONTHLY   => Some("Monthly")
            case RecurringPayment.RecurringPaymentFrequency.BIMONTHLY => Some("Bimonthly")
            case RecurringPayment.RecurringPaymentFrequency.QUARTERLY => Some("Quarterly")
            case RecurringPayment.RecurringPaymentFrequency.BIANNUAL  => Some("Semiannual")
            case RecurringPayment.RecurringPaymentFrequency.ANNUAL    => Some("Annual")
            case _                                                    => None
          }
        case _ => None
      }) match {
        case Some(frequency) => createRecurringPayment.setFrequency(frequency)
        case _               =>
      }
      recurringPayment.fixedNextAmount match {
        case Some(fixedNextAmount) => createRecurringPayment.setFixedNextAmount(fixedNextAmount)
        case _                     =>
      }
      recurringPayment.nextDebitedAmount match {
        case Some(nextDebitedAmount) =>
          val nextTransactionDebitedFunds = new Money()
          nextTransactionDebitedFunds.setAmount(nextDebitedAmount)
          nextTransactionDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
          createRecurringPayment.setNextTransactionDebitedFunds(nextTransactionDebitedFunds)
        case _ =>
      }
      recurringPayment.nextFeesAmount match {
        case Some(nextFeesAmount) =>
          val nextTransactionFees = new Money()
          nextTransactionFees.setAmount(nextFeesAmount)
          nextTransactionFees.setCurrency(CurrencyIso.valueOf(currency))
          createRecurringPayment.setNextTransactionFees(nextTransactionFees)
        case _ =>
      }
      recurringPayment.migration match {
        case Some(migration) => createRecurringPayment.setMigration(migration)
        case _               =>
      }

      val registration =
        RecurringCardPaymentRegistration(
          generateUUID(),
          "created",
          RecurringCardPaymentState.defaultInstance,
          createRecurringPayment
        )

      RecurringCardPaymentRegistrations = RecurringCardPaymentRegistrations.updated(
        registration.id,
        registration
      )

      Some(
        RecurringPayment.RecurringCardPaymentResult.defaultInstance
          .withId(registration.id)
          .withStatus(registration.status)
      )
    } else {
      None
    }
  }

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @param cardId
    *   - Provider card id
    * @param status
    *   - optional recurring payment status
    * @return
    *   recurring card payment registration updated result
    */
  override def updateRecurringCardPaymentRegistration(
    recurringPayInRegistrationId: String,
    cardId: Option[String],
    status: Option[RecurringPayment.RecurringCardPaymentStatus]
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    if (cardId.isDefined || status.isDefined) {
      RecurringCardPaymentRegistrations.get(recurringPayInRegistrationId) match {
        case Some(recurringCardPaymentRegistration) =>
          val updatedRecurringCardPaymentRegistration =
            recurringCardPaymentRegistration.copy(
              status = status.map(_.name).getOrElse(recurringCardPaymentRegistration.status)
            )
          updatedRecurringCardPaymentRegistration.registration.setCardId(
            cardId.getOrElse(recurringCardPaymentRegistration.registration.getCardId)
          )
          RecurringCardPaymentRegistrations = RecurringCardPaymentRegistrations.updated(
            recurringPayInRegistrationId,
            updatedRecurringCardPaymentRegistration
          )
          Some(
            RecurringPayment.RecurringCardPaymentResult.defaultInstance
              .withId(recurringPayInRegistrationId)
              .withStatus(updatedRecurringCardPaymentRegistration.status)
              .withCurrentstate(updatedRecurringCardPaymentRegistration.currentState)
          )
        case _ => None
      }
    } else {
      None
    }
  }

  /** @param recurringPayInRegistrationId
    *   - recurring payIn registration id
    * @return
    *   recurring card payment registration result
    */
  override def loadRecurringCardPayment(
    recurringPayInRegistrationId: String
  ): Option[RecurringPayment.RecurringCardPaymentResult] = {
    RecurringCardPaymentRegistrations.get(recurringPayInRegistrationId) match {
      case Some(recurringCardPaymentRegistration) =>
        Some(
          RecurringPayment.RecurringCardPaymentResult.defaultInstance
            .withId(recurringPayInRegistrationId)
            .withStatus(recurringCardPaymentRegistration.status)
            .withCurrentstate(recurringCardPaymentRegistration.currentState)
        )
      case _ => None
    }
  }

  /** @param recurringPaymentTransaction
    *   - recurring payment transaction
    * @return
    *   resulted payIn transaction
    */
  override def createRecurringCardPayment(
    recurringPaymentTransaction: RecurringPaymentTransaction
  ): Option[Transaction] = {
    RecurringCardPaymentRegistrations.get(
      recurringPaymentTransaction.recurringPayInRegistrationId
    ) match {
      case Some(recurringPaymentRegistration) =>
        recurringPaymentTransaction.extension[Option[FirstRecurringPaymentTransaction]](
          FirstRecurringPaymentTransaction.first
        ) match {
          case Some(firstRecurringPaymentTransaction) =>
            import recurringPaymentTransaction._
            val recurringPayInCIT: RecurringPayInCIT = new RecurringPayInCIT
            recurringPayInCIT.setRecurringPayInRegistrationId(recurringPayInRegistrationId)
            recurringPayInCIT.setIpAddress(firstRecurringPaymentTransaction.ipAddress.getOrElse(""))
            val debitedFunds = new Money
            debitedFunds.setAmount(debitedAmount)
            debitedFunds.setCurrency(CurrencyIso.valueOf(currency))
            recurringPayInCIT.setDebitedFunds(debitedFunds)
            val fees = new Money
            fees.setAmount(feesAmount)
            fees.setCurrency(CurrencyIso.valueOf(currency))
            recurringPayInCIT.setFees(fees)
            recurringPayInCIT.setTag(externalUuid)
            recurringPayInCIT.setStatementDescriptor(statementDescriptor)
            recurringPayInCIT.setSecureModeReturnURL(
              s"${config.recurringPaymentReturnUrl}/$recurringPayInRegistrationId"
            )

            import recurringPaymentRegistration._

            val payIn = new PayIn()
            payIn.setTag(externalUuid)
            payIn.setCreditedWalletId(registration.getCreditedWalletId)
            payIn.setAuthorId(registration.getAuthorId)
            payIn.setDebitedFunds(recurringPayInCIT.getDebitedFunds)
            payIn.setFees(recurringPayInCIT.getFees)
            payIn.setPaymentType(PayInPaymentType.CARD)
            val paymentDetails = new PayInPaymentDetailsCard
            paymentDetails.setCardId(registration.getCardId)
            paymentDetails.setCardType(CardType.CB_VISA_MASTERCARD)
            paymentDetails.setStatementDescriptor(recurringPayInCIT.getStatementDescriptor)
            payIn.setPaymentDetails(paymentDetails)
            payIn.setExecutionType(PayInExecutionType.DIRECT)
            val executionDetails = new PayInExecutionDetailsDirect
            executionDetails.setCardId(registration.getCardId)
            // Secured Mode is activated from €100.
            executionDetails.setSecureMode(SecureMode.DEFAULT)
            executionDetails.setSecureModeReturnUrl(recurringPayInCIT.getSecureModeReturnURL)
            payIn.setExecutionDetails(executionDetails)

            payIn.setId(generateUUID())
            payIn.setStatus(MangoPayTransactionStatus.CREATED)
            payIn.setResultCode(OK)
            payIn.setResultMessage(CREATED)
            executionDetails.setSecureModeRedirectUrl(
              s"${executionDetails.getSecureModeReturnUrl}&transactionId=${payIn.getId}"
            )

            val previousState = recurringPaymentRegistration.currentState
            RecurringCardPaymentRegistrations = RecurringCardPaymentRegistrations.updated(
              recurringPayInRegistrationId,
              recurringPaymentRegistration.copy(
                status = "pending",
                currentState = RecurringCardPaymentState(
                  previousState.numberOfRecurringPayments + 1,
                  previousState.cumulatedDebitedAmount + debitedAmount,
                  previousState.cumulatedFeesAmount + feesAmount,
                  payIn.getId
                )
              )
            )
            PayIns = PayIns.updated(payIn.getId, payIn)

            Some(
              Transaction().copy(
                id = payIn.getId,
                nature = Transaction.TransactionNature.REGULAR,
                `type` = Transaction.TransactionType.PAYIN,
                status = payIn.getStatus,
                amount = debitedAmount,
                cardId = Option(registration.getCardId),
                fees = feesAmount,
                resultCode = payIn.getResultCode,
                resultMessage = payIn.getResultMessage,
                redirectUrl =
                  if (debitedAmount > 5000) Option(executionDetails.getSecureModeRedirectUrl)
                  else None,
                authorId = registration.getAuthorId,
                creditedWalletId = Some(registration.getCreditedWalletId),
                recurringPayInRegistrationId = Option(recurringPayInRegistrationId)
              )
            )
          case _ =>
            import recurringPaymentTransaction._
            val recurringPayInMIT: RecurringPayInMIT = new RecurringPayInMIT
            recurringPayInMIT.setRecurringPayInRegistrationId(recurringPayInRegistrationId)
            recurringPayInMIT.setStatementDescriptor(statementDescriptor)
            val debitedFunds = new Money
            debitedFunds.setAmount(debitedAmount)
            debitedFunds.setCurrency(CurrencyIso.valueOf(currency))
            recurringPayInMIT.setDebitedFunds(debitedFunds)
            val fees = new Money
            fees.setAmount(feesAmount)
            fees.setCurrency(CurrencyIso.valueOf(currency))
            recurringPayInMIT.setFees(fees)
            recurringPayInMIT.setTag(externalUuid)

            import recurringPaymentRegistration._

            val payIn = new PayIn()
            payIn.setTag(externalUuid)
            payIn.setCreditedWalletId(registration.getCreditedWalletId)
            payIn.setAuthorId(registration.getAuthorId)
            payIn.setDebitedFunds(recurringPayInMIT.getDebitedFunds)
            payIn.setFees(recurringPayInMIT.getFees)
            payIn.setPaymentType(PayInPaymentType.CARD)
            val paymentDetails = new PayInPaymentDetailsCard
            paymentDetails.setCardId(registration.getCardId)
            paymentDetails.setCardType(CardType.CB_VISA_MASTERCARD)
            paymentDetails.setStatementDescriptor(recurringPayInMIT.getStatementDescriptor)
            payIn.setPaymentDetails(paymentDetails)
            payIn.setExecutionType(PayInExecutionType.DIRECT)
            val executionDetails = new PayInExecutionDetailsDirect
            executionDetails.setCardId(registration.getCardId)
            payIn.setExecutionDetails(executionDetails)

            payIn.setId(generateUUID())
            payIn.setStatus(MangoPayTransactionStatus.CREATED)
            payIn.setResultCode(OK)
            payIn.setResultMessage(CREATED)
            executionDetails.setSecureModeRedirectUrl(
              s"${executionDetails.getSecureModeReturnUrl}&transactionId=${payIn.getId}"
            )

            val previousState = recurringPaymentRegistration.currentState
            RecurringCardPaymentRegistrations = RecurringCardPaymentRegistrations.updated(
              recurringPayInRegistrationId,
              recurringPaymentRegistration.copy(
                status = "pending",
                currentState = RecurringCardPaymentState(
                  previousState.numberOfRecurringPayments + 1,
                  previousState.cumulatedDebitedAmount + debitedAmount,
                  previousState.cumulatedFeesAmount + feesAmount,
                  payIn.getId
                )
              )
            )
            PayIns = PayIns.updated(payIn.getId, payIn)

            Some(
              Transaction().copy(
                id = payIn.getId,
                nature = Transaction.TransactionNature.REGULAR,
                `type` = Transaction.TransactionType.PAYIN,
                status = payIn.getStatus,
                amount = debitedAmount,
                cardId = Option(registration.getCardId),
                fees = feesAmount,
                resultCode = payIn.getResultCode,
                resultMessage = payIn.getResultMessage,
                redirectUrl = None,
                authorId = registration.getAuthorId,
                creditedWalletId = Some(registration.getCreditedWalletId),
                recurringPayInRegistrationId = Option(recurringPayInRegistrationId)
              )
            )
        }
      case _ => None
    }
  }
}

object MockMangoPayProvider {
  var Users: Map[String, UserNatural] = Map.empty

  var LegalUsers: Map[String, UserLegal] = Map.empty

  var Wallets: Map[String, Wallet] = Map.empty

  var BankAccounts: Map[String, MangoPayBankAccount] = Map.empty

  var CardRegistrations: Map[String, CardRegistration] = Map.empty

  var Cards: Map[String, Card] = Map.empty

  var PayIns: Map[String, PayIn] = Map.empty

  var CardPreAuthorizations: Map[String, CardPreAuthorization] = Map.empty

  var PayOuts: Map[String, PayOut] = Map.empty

  var Refunds: Map[String, Refund] = Map.empty

  var Transfers: Map[String, Transfer] = Map.empty

  var Mandates: Map[String, Mandate] = Map.empty

  var Documents: Map[String, KycDocumentValidationReport] = Map.empty

  var UboDeclarations: Map[String, UboDeclaration] = Map.empty

  var RecurringCardPaymentRegistrations: Map[String, RecurringCardPaymentRegistration] = Map.empty

  var ClientFees: Double = 0d

}

case class RecurringCardPaymentRegistration(
  id: String,
  status: String,
  currentState: RecurringCardPaymentState,
  registration: CreateRecurringPayment
)

case class MockMangoPayConfig(config: MangoPayConfig)
    extends ProviderConfig(
      config.clientId,
      config.apiKey,
      config.baseUrl,
      config.version,
      config.debug,
      config.secureModePath,
      config.hooksPath,
      config.mandatePath,
      config.paypalPath
    )
    with MangoPayConfig {
  override def `type`: Provider.ProviderType = Provider.ProviderType.MOCK
  override val technicalErrors: Set[String] = config.technicalErrors

  override def paymentConfig: Payment.Config = config.paymentConfig

  override def withPaymentConfig(paymentConfig: Payment.Config): MangoPayConfig =
    this.copy(config = config.withPaymentConfig(paymentConfig))
}

class MockMangoPayProviderFactory extends PaymentProviderSpi {
  @volatile private[this] var _config: Option[MangoPayConfig] = None

  override val providerType: Provider.ProviderType =
    Provider.ProviderType.MOCK

  override def paymentProvider(p: Client.Provider): MockMangoPayProvider =
    new MockMangoPayProvider {
      override implicit def provider: Provider = p
      override implicit def config: MangoPayConfig =
        _config.getOrElse(MockMangoPayConfig(MangoPaySettings.MangoPayConfig))
    }

  override def softPaymentProvider(config: Config): Provider = {
    val mangoPayConfig = MockMangoPayConfig(MangoPaySettings(config).MangoPayConfig)
    _config = Some(mangoPayConfig)
    mangoPayConfig.softPayProvider.withProviderType(providerType)
  }

  override def hooksDirectives(implicit
    _system: ActorSystem[_],
    _formats: Formats
  ): HooksDirectives =
    new MangoPayHooksDirectives with MockPaymentHandler {
      override implicit def system: ActorSystem[_] = _system
      override implicit def formats: Formats = _formats
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
    }

  override def hooksEndpoints(implicit _system: ActorSystem[_], formats: Formats): HooksEndpoints =
    new MangoPayHooksEndpoints with MockPaymentHandler {
      override implicit def system: ActorSystem[_] = _system
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
    }
}
