package app.softnetwork.payment.spi

import akka.actor.typed.ActorSystem
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.config.MangoPay.MangoPayConfig

import java.text.SimpleDateFormat
import java.util
import java.util.{Calendar, Date, TimeZone}
import com.mangopay.core.{Address => MangoPayAddress, _}
import com.mangopay.core.enumerations.{
  TransactionNature => MangoPayTransactionNature,
  TransactionStatus => MangoPayTransactionStatus,
  TransactionType => MangoPayTransactionType,
  _
}
import com.mangopay.entities.{
  BankAccount => MangoPayBankAccount,
  Birthplace => MangoPayBirthplace,
  Card => _,
  KycDocument => _,
  Transaction => _,
  UboDeclaration => _,
  _
}
import com.mangopay.entities.subentities.{BrowserInfo => MangoPayBrowserInfo, _}
import app.softnetwork.payment.model.{RecurringPayment, _}
import app.softnetwork.payment.config.{MangoPay, MangoPaySettings}
import app.softnetwork.payment.model.NaturalUser.NaturalUserType
import app.softnetwork.payment.model.SoftPayAccount.Client.Provider
import app.softnetwork.payment.service.{
  HooksDirectives,
  HooksEndpoints,
  MangoPayHooksDirectives,
  MangoPayHooksEndpoints
}

import scala.util.{Failure, Success, Try}
import app.softnetwork.persistence._
import app.softnetwork.serialization.asJson
import app.softnetwork.time.{epochSecondToDate, epochSecondToLocalDate, DateExtensions}
import com.typesafe.config.Config
import org.json4s.Formats

import java.time.format.DateTimeFormatter
import scala.language.implicitConversions

/** a payment provider for MangoPay
  */
trait MangoPayProvider extends PaymentProvider {

  import scala.language.implicitConversions
  import scala.collection.JavaConverters._

  override implicit def config: MangoPayConfig

  implicit def computeBirthday(epochSecond: Long): String = {
    epochSecondToLocalDate(epochSecond).format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
  }

  implicit def mangoPayTransactionStatusToTransactionStatus(
    status: MangoPayTransactionStatus
  ): Transaction.TransactionStatus =
    status match {
      case MangoPayTransactionStatus.CREATED => Transaction.TransactionStatus.TRANSACTION_CREATED
      case MangoPayTransactionStatus.FAILED  => Transaction.TransactionStatus.TRANSACTION_FAILED
      case MangoPayTransactionStatus.SUCCEEDED =>
        Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
      case _ => Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED
    }

  implicit def preAuthorizationStatusToTransactionStatus(
    status: PreAuthorizationStatus
  ): Transaction.TransactionStatus =
    status match {
      case PreAuthorizationStatus.CREATED   => Transaction.TransactionStatus.TRANSACTION_CREATED
      case PreAuthorizationStatus.FAILED    => Transaction.TransactionStatus.TRANSACTION_FAILED
      case PreAuthorizationStatus.SUCCEEDED => Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
      case _ => Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED
    }

  implicit def kycStatusToKycDocumentStatus(status: KycStatus): KycDocument.KycDocumentStatus =
    status match {
      case KycStatus.CREATED          => KycDocument.KycDocumentStatus.KYC_DOCUMENT_CREATED
      case KycStatus.OUT_OF_DATE      => KycDocument.KycDocumentStatus.KYC_DOCUMENT_OUT_OF_DATE
      case KycStatus.REFUSED          => KycDocument.KycDocumentStatus.KYC_DOCUMENT_REFUSED
      case KycStatus.VALIDATION_ASKED => KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATION_ASKED
      case KycStatus.VALIDATED        => KycDocument.KycDocumentStatus.KYC_DOCUMENT_VALIDATED
      case _                          => KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED
    }

  implicit def mangoPayMandateStatusToMandateStatus(
    status: MandateStatus
  ): BankAccount.MandateStatus =
    status match {
      case MandateStatus.ACTIVE    => BankAccount.MandateStatus.MANDATE_ACTIVATED
      case MandateStatus.CREATED   => BankAccount.MandateStatus.MANDATE_CREATED
      case MandateStatus.EXPIRED   => BankAccount.MandateStatus.MANDATE_EXPIRED
      case MandateStatus.FAILED    => BankAccount.MandateStatus.MANDATE_FAILED
      case MandateStatus.SUBMITTED => BankAccount.MandateStatus.MANDATE_SUBMITTED
      case _                       => BankAccount.MandateStatus.Unrecognized(-1)
    }

  implicit def legalUserTypeToLegalPersonType(
    legalUserType: LegalUser.LegalUserType
  ): LegalPersonType = {
    legalUserType match {
      case LegalUser.LegalUserType.BUSINESS     => LegalPersonType.BUSINESS
      case LegalUser.LegalUserType.ORGANIZATION => LegalPersonType.ORGANIZATION
      case LegalUser.LegalUserType.SOLETRADER   => LegalPersonType.SOLETRADER
      case _                                    => LegalPersonType.NotSpecified
    }
  }

  implicit def kycDocumentTypeToMangopayKycDocumentType(
    documentType: KycDocument.KycDocumentType
  ): KycDocumentType = {
    documentType match {
      case KycDocument.KycDocumentType.KYC_IDENTITY_PROOF     => KycDocumentType.IDENTITY_PROOF
      case KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF => KycDocumentType.REGISTRATION_PROOF
      case KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION =>
        KycDocumentType.ARTICLES_OF_ASSOCIATION
      case KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION =>
        KycDocumentType.SHAREHOLDER_DECLARATION
      case _ => KycDocumentType.NotSpecified
    }
  }

  implicit def MangopayKycDocumentTypeTokycDocumentType(
    documentType: KycDocumentType
  ): KycDocument.KycDocumentType = {
    documentType match {
      case KycDocumentType.IDENTITY_PROOF     => KycDocument.KycDocumentType.KYC_IDENTITY_PROOF
      case KycDocumentType.REGISTRATION_PROOF => KycDocument.KycDocumentType.KYC_REGISTRATION_PROOF
      case KycDocumentType.ARTICLES_OF_ASSOCIATION =>
        KycDocument.KycDocumentType.KYC_ARTICLES_OF_ASSOCIATION
      case KycDocumentType.SHAREHOLDER_DECLARATION =>
        KycDocument.KycDocumentType.KYC_SHAREHOLDER_DECLARATION
      case KycDocumentType.ADDRESS_PROOF => KycDocument.KycDocumentType.KYC_ADDRESS_PROOF
      case KycDocumentType.NotSpecified  => KycDocument.KycDocumentType.Unrecognized(-1)
    }
  }

  implicit def mangoPayUboDeclarationStatus2DeclarationStatus(
    status: UboDeclarationStatus
  ): UboDeclaration.UboDeclarationStatus = {
    status match {
      case UboDeclarationStatus.CREATED =>
        UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_CREATED
      case UboDeclarationStatus.REFUSED =>
        UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_REFUSED
      case UboDeclarationStatus.VALIDATED =>
        UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATED
      case UboDeclarationStatus.VALIDATION_ASKED =>
        UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_VALIDATION_ASKED
      case UboDeclarationStatus.INCOMPLETE =>
        UboDeclaration.UboDeclarationStatus.UBO_DECLARATION_INCOMPLETE
      case _ => UboDeclaration.UboDeclarationStatus.Unrecognized(-1)
    }
  }

  implicit def mangoPayRecurringPaymentStatus2RecurringCardPaymentStatus(
    status: String
  ): RecurringPayment.RecurringCardPaymentStatus = {
    status.toUpperCase match {
      case RecurringPayment.RecurringCardPaymentStatus.CREATED.name =>
        RecurringPayment.RecurringCardPaymentStatus.CREATED
      case RecurringPayment.RecurringCardPaymentStatus.AUTHENTICATION_NEEDED.name =>
        RecurringPayment.RecurringCardPaymentStatus.AUTHENTICATION_NEEDED
      case RecurringPayment.RecurringCardPaymentStatus.IN_PROGRESS.name =>
        RecurringPayment.RecurringCardPaymentStatus.IN_PROGRESS
      case RecurringPayment.RecurringCardPaymentStatus.ENDED.name =>
        RecurringPayment.RecurringCardPaymentStatus.ENDED
      case _ => RecurringPayment.RecurringCardPaymentStatus.Unrecognized(-1)
    }
  }

  implicit def mangoPayCurrentState2RecurringCardPaymentState(
    currentState: CurrentState
  ): RecurringPayment.RecurringCardPaymentState = {
    RecurringPayment.RecurringCardPaymentState.defaultInstance
      .withNumberOfRecurringPayments(currentState.getPayinsLinked)
      .withCumulatedDebitedAmount(currentState.getCumulatedDebitedAmount.getAmount)
      .withCumulatedFeesAmount(currentState.getCumulatedFeesAmount.getAmount)
      .copy(
        lastPayInTransactionId = Option(currentState.getLastPayinId).getOrElse("")
      )
  }

  implicit def mangoPayPaymentTypeToTransactionPaymentType(
    paymentType: PayInPaymentType
  ): Transaction.PaymentType = {
    paymentType match {
      case PayInPaymentType.CARD          => Transaction.PaymentType.CARD
      case PayInPaymentType.PAYPAL        => Transaction.PaymentType.PAYPAL
      case PayInPaymentType.DIRECT_DEBIT  => Transaction.PaymentType.DIRECT_DEBITED
      case PayInPaymentType.PREAUTHORIZED => Transaction.PaymentType.PREAUTHORIZED
      case PayInPaymentType.BANK_WIRE     => Transaction.PaymentType.BANK_WIRE
      case PayInPaymentType.APPLEPAY      => Transaction.PaymentType.APPLEPAY
      case PayInPaymentType.GOOGLEPAY     => Transaction.PaymentType.GOOGLEPAY
      case _                              => Transaction.PaymentType.Unrecognized(-1)
    }
  }

  implicit def languageToCultureCode(language: Option[String]): CultureCode = {
    Try(
      CultureCode.valueOf(language.getOrElse("FR").toUpperCase())
    ) match {
      case Success(value) => value
      case Failure(f) =>
        mlog.warn(f.getMessage)
        CultureCode.FR
    }
  }

  /** @param maybeNaturalUser
    *   - natural user to create
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] def createOrUpdateNaturalUser(
    maybeNaturalUser: Option[NaturalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String]
  ): Option[String] = {
    maybeNaturalUser match {
      case Some(naturalUser) =>
        import naturalUser._
        val sdf = new SimpleDateFormat("dd/MM/yyyy")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        Try(sdf.parse(birthday)) match {
          case Success(s) =>
            val user = new UserNatural
            user.setId(userId.getOrElse(""))
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
            (if (userId.getOrElse("").trim.isEmpty)
               None
             else
               Try(MangoPay().getUserApi.getNatural(userId.getOrElse(""))) match {
                 case Success(u) => Option(u)
                 case Failure(f) =>
                   mlog.error(f.getMessage, f)
                   None
               }) match {
              case Some(u) =>
                user.setId(u.getId)
                Try(MangoPay().getUserApi.update(user).getId) match {
                  case Success(id) => Some(id)
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                    None
                }
              case None =>
                Try(MangoPay().getUserApi.create(user).getId) match {
                  case Success(id) => Some(id)
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                    None
                }
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case None => None
    }
  }

  /** @param maybeLegalUser
    *   - legal user to create
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] def createOrUpdateLegalUser(
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
            (if (legalRepresentative.userId.isEmpty)
               None
             else
               Try(
                 MangoPay().getUserApi.getLegal(legalRepresentative.userId.getOrElse(""))
               ) match {
                 case Success(u) => Option(u)
                 case Failure(f) =>
                   mlog.error(f.getMessage, f)
                   None
               }) match {
              case Some(u) =>
                user.setId(u.getId)
                Try(MangoPay().getUserApi.update(user).getId) match {
                  case Success(id) => Some(id)
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                    None
                }
              case None =>
                Try(MangoPay().getUserApi.create(user).getId) match {
                  case Success(id) => Some(id)
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                    None
                }
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
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
  def createOrUpdateWallet(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String,
    maybeWalletId: Option[String]
  ): Option[String] = {
    maybeUserId match {
      case Some(userId) =>
        val wallet = new Wallet
        wallet.setCurrency(CurrencyIso.valueOf(currency))
        wallet.setOwners(new util.ArrayList(List(userId).asJava))
        wallet.setDescription(s"wallet for $externalUuid")
        wallet.setTag(externalUuid)
        Try(
          MangoPay().getUserApi
            .getWallets(userId)
            .asScala
            .find(w =>
              (maybeWalletId.isDefined && w.getId == maybeWalletId.get) || w.getTag == externalUuid
            )
        ) match {
          case Success(maybeWallet) =>
            maybeWallet match {
              case Some(w) =>
                wallet.setId(w.getId)
                Try(MangoPay().getWalletApi.update(wallet).getId) match {
                  case Success(id) => Some(id)
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                    None
                }
              case None =>
                Try(MangoPay().getWalletApi.create(wallet).getId) match {
                  case Success(id) => Some(id)
                  case Failure(f) =>
                    mlog.error(f.getMessage, f)
                    None
                }
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case None => None
    }
  }

  /** @param maybeBankAccount
    *   - bank account to create
    * @return
    *   bank account id
    */
  def createOrUpdateBankAccount(maybeBankAccount: Option[BankAccount]): Option[String] = {
    maybeBankAccount match {
      case Some(mangoPayBankAccount) =>
        import mangoPayBankAccount._
        val bankAccount = new MangoPayBankAccount
        if (id.isDefined) {
          bankAccount.setId(getId)
        }
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
        (if (id.isDefined)
           Try(MangoPay().getUserApi.getBankAccount(userId, getId)) match {
             case Success(b) => Option(b)
             case Failure(f) =>
               mlog.error(f.getMessage, f)
               None
           }
         else None) match {
          case Some(previousBankAccount) =>
            Try(
              MangoPay().getUserApi
                .updateBankAccount(userId, bankAccount, previousBankAccount.getId)
                .getId
            ) match {
              case Success(id) => Option(id)
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
          case _ =>
            Try(MangoPay().getUserApi.createBankAccount(userId, bankAccount).getId) match {
              case Success(id) => Option(id)
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
        }
      case None => None
    }
  }

  /** @param userId
    *   - provider user id
    * @return
    *   the first active bank account
    */
  override def getActiveBankAccount(userId: String, currency: String): Option[String] = {
    Try(
      MangoPay().getUserApi
        .getBankAccounts(userId)
        .asScala
        .filter(bankAccount => bankAccount.isActive)
    ) match {
      case Success(maybeBankAccount) =>
        maybeBankAccount match {
          case bas if bas.nonEmpty =>
            Some(bas.toList.sortWith(_.getCreationDate > _.getCreationDate).head.getId)
          case _ => None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param maybeUserId
    *   - owner of the card
    * @param externalUuid
    *   - external unique id
    * @return
    *   card pre registration
    */
  def preRegisterCard(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String
  ): Option[CardPreRegistration] = {
    maybeUserId match {
      case Some(userId) =>
        val cardPreRegistration = new CardRegistration()
        cardPreRegistration.setCurrency(CurrencyIso.valueOf(currency))
        cardPreRegistration.setTag(externalUuid)
        cardPreRegistration.setUserId(userId)
        Try(MangoPay().getCardRegistrationApi.create(cardPreRegistration)) match {
          case Success(cardRegistration) =>
            Some(
              CardPreRegistration.defaultInstance
                .withId(cardRegistration.getId)
                .withAccessKey(cardRegistration.getAccessKey)
                .withPreregistrationData(cardRegistration.getPreregistrationData)
                .withCardRegistrationURL(cardRegistration.getCardRegistrationUrl)
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case None =>
        None
    }
  }

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
  ): Option[String] = {
    maybeRegistrationData match {
      case Some(registrationData) =>
        Try(MangoPay().getCardRegistrationApi.get(cardPreRegistrationId)) match {
          case Success(cardRegistration) =>
            cardRegistration.setRegistrationData(registrationData)
            Try(
              MangoPay().getCardRegistrationApi.update(cardRegistration).getCardId
            ) match {
              case Success(cardId) =>
                Option(cardId)
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case None =>
        None
    }
  }

  /** @param cardId
    *   - card id
    * @return
    *   card
    */
  def loadCard(cardId: String): Option[Card] = {
    Try(MangoPay().getCardApi.get(cardId)) match {
      case Success(card) =>
        Some(
          Card.defaultInstance
            .withId(cardId)
            .withAlias(card.getAlias)
            .withExpirationDate(card.getExpirationDate)
            .withActive(card.isActive)
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param cardId
    *   - the id of the card to disable
    * @return
    *   the card disabled or none
    */
  override def disableCard(cardId: String): Option[Card] = {
    Try(MangoPay().getCardApi.get(cardId)) match {
      case Success(card) =>
        Try(MangoPay().getCardApi.disable(card)) match {
          case Success(disabledCard) =>
            Some(
              Card.defaultInstance
                .withId(cardId)
                .withAlias(card.getAlias)
                .withExpirationDate(card.getExpirationDate)
                .withActive(disabledCard.isActive)
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
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
    if (browserInfo.isDefined) {
      val bi = browserInfo.get
      import bi._
      val mangoPayBrowserInfo = new MangoPayBrowserInfo()
      mangoPayBrowserInfo.setAcceptHeader(acceptHeader)
      mangoPayBrowserInfo.setColorDepth(colorDepth)
      mangoPayBrowserInfo.setJavaEnabled(javaEnabled)
      mangoPayBrowserInfo.setJavascriptEnabled(javascriptEnabled)
      mangoPayBrowserInfo.setLanguage(language)
      mangoPayBrowserInfo.setScreenHeight(screenHeight)
      mangoPayBrowserInfo.setScreenWidth(screenWidth)
      mangoPayBrowserInfo.setTimeZoneOffset(timeZoneOffset)
      mangoPayBrowserInfo.setUserAgent(userAgent)
      cardPreAuthorization.setBrowserInfo(mangoPayBrowserInfo)
    }
    cardPreAuthorization.setCardId(cardId)
    cardPreAuthorization.setDebitedFunds(new Money)
    cardPreAuthorization.getDebitedFunds.setAmount(debitedAmount)
    cardPreAuthorization.getDebitedFunds.setCurrency(CurrencyIso.valueOf(currency))
    cardPreAuthorization.setExecutionType(PreAuthorizationExecutionType.DIRECT)
    if (ipAddress.isDefined) {
      cardPreAuthorization.setIpAddress(ipAddress.get)
    }
    cardPreAuthorization.setSecureMode(SecureMode.DEFAULT)
    cardPreAuthorization.setSecureModeReturnUrl(
      s"${config.preAuthorizeCardReturnUrl}/$orderUuid?registerCard=${registerCard
        .getOrElse(false)}&printReceipt=${printReceipt.getOrElse(false)}"
    )
    cardPreAuthorization.setPaymentStatus(PaymentStatus.WAITING)
    val idempotencyKey =
      idempotency match {
        case Some(s) if s => Some(generateUUID())
        case _            => None
      }
    Try(
      idempotencyKey match {
        case Some(s) =>
          MangoPay().getCardPreAuthorizationApi.create(s, cardPreAuthorization)
        case _ => MangoPay().getCardPreAuthorizationApi.create(cardPreAuthorization)
      }
    ) match {
      case Success(result) =>
        mlog.info("preAuthorizeCard -> " + asJson(result))
        Some(
          Transaction().copy(
            id = result.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PRE_AUTHORIZATION,
            status = result.getStatus match {
              case PreAuthorizationStatus.FAILED if Option(result.getResultCode).isDefined =>
                if (config.technicalErrors.contains(result.getResultCode))
                  Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                else
                  Transaction.TransactionStatus.TRANSACTION_FAILED
              case other => other
            },
            amount = debitedAmount,
            cardId = if (cardId.trim.isEmpty) None else Some(cardId),
            fees = 0,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
            redirectUrl = Option( // for 3D Secure
              result.getSecureModeRedirectUrl
            ),
            authorId = result.getAuthorId,
            paymentType = Transaction.PaymentType.CARD,
            idempotencyKey = idempotencyKey
          )
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
      /*
                      f match {
                        case r: ResponseException =>
                          Some(
                            Transaction().copy(
                              uuid = r.getId,
                              id = r.getId,
                              orderUuid = orderUuid,
                              nature = Transaction.TransactionNature.REGULAR,
                              `type` = Transaction.TransactionType.PRE_AUTHORIZATION,
                              status = MangoPayTransactionStatus.NotSpecified,
                              amount = debitedAmount,
                              cardId = Some(cardId),
                              fees = 0,
                              resultCode = r.getType,
                              resultMessage = r.getApiMessage,
                              redirectUrl = None
                            )
                          )
                        case _ =>
                          mlog.error(f.getMessage, f)
                          None
                      }
       */
    }
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
    Try(
      MangoPay().getCardPreAuthorizationApi.get(cardPreAuthorizedTransactionId)
    ) match {
      case Success(result) =>
        Some(
          Transaction().copy(
            id = result.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PRE_AUTHORIZATION,
            status = result.getStatus match {
              case PreAuthorizationStatus.FAILED if Option(result.getResultCode).isDefined =>
                if (config.technicalErrors.contains(result.getResultCode))
                  Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                else
                  Transaction.TransactionStatus.TRANSACTION_FAILED
              case other => other
            },
            amount = result.getDebitedFunds.getAmount,
            cardId = Option(result.getCardId),
            fees = 0,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
            redirectUrl = Option( // for 3D Secure
              result.getSecureModeRedirectUrl
            ),
            authorId = result.getAuthorId,
            preAuthorizationCanceled = Option(result.getPaymentStatus == PaymentStatus.CANCELED),
            preAuthorizationValidated = Option(result.getPaymentStatus == PaymentStatus.VALIDATED),
            preAuthorizationExpired = Option(result.getPaymentStatus == PaymentStatus.EXPIRED)
          )
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
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
      case Some(payInWithCardPreAuthorizedTransaction)
          if Option(
            payInWithCardPreAuthorizedTransaction.cardPreAuthorizedTransactionId
          ).isDefined =>
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
        val idempotencyKey =
          idempotency match {
            case Some(s) if s => Some(generateUUID())
            case _            => None
          }
        Try(
          idempotencyKey match {
            case Some(s) => MangoPay().getPayInApi.create(s, payIn)
            case _       => MangoPay().getPayInApi.create(payIn)
          }
        ) match {
          case Success(result) =>
            Some(
              Transaction()
                .copy(
                  id = result.getId,
                  orderUuid = orderUuid,
                  nature = Transaction.TransactionNature.REGULAR,
                  `type` = Transaction.TransactionType.PAYIN,
                  status = result.getStatus match {
                    case MangoPayTransactionStatus.FAILED
                        if Option(result.getResultCode).isDefined =>
                      if (
                        config.technicalErrors
                          .contains(result.getResultCode)
                      )
                        Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                      else
                        Transaction.TransactionStatus.TRANSACTION_FAILED
                    case other => other
                  },
                  amount = debitedAmount,
                  cardId = None,
                  fees = 0,
                  resultCode = Option(result.getResultCode).getOrElse(""),
                  resultMessage = Option(result.getResultMessage).getOrElse(""),
                  redirectUrl = None,
                  authorId = result.getAuthorId,
                  creditedWalletId = Option(result.getCreditedWalletId),
                  idempotencyKey = idempotencyKey
                )
                .withPaymentType(Transaction.PaymentType.PREAUTHORIZED)
                .withPreAuthorizationId(cardPreAuthorizedTransactionId)
                .withPreAuthorizationDebitedAmount(preAuthorizationDebitedAmount)
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
          /*
                          f match {
                            case r: ResponseException =>
                              Some(
                                Transaction().copy(
                                  uuid = r.getId,
                                  id = r.getId,
                                  orderUuid = orderUuid,
                                  nature = Transaction.TransactionNature.REGULAR,
                                  `type` = Transaction.TransactionType.PAYIN,
                                  status = MangoPayTransactionStatus.NotSpecified,
                                  amount = debitedAmount,
                                  cardId = Some(cardId),
                                  fees = 0,
                                  resultCode = r.getType,
                                  resultMessage = r.getApiMessage,
                                  redirectUrl = None
                                )
                              )
                            case _ =>
                              mlog.error(f.getMessage, f)
                              None
                          }
           */
        }
      case None =>
        None
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
    Try(
      MangoPay().getCardPreAuthorizationApi.get(cardPreAuthorizedTransactionId)
    ) match {
      case Success(result) =>
        result.setPaymentStatus(PaymentStatus.CANCELED)
        Try(
          MangoPay().getCardPreAuthorizationApi.update(result)
        ) match {
          case Success(_) => true
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            false
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        false
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param cardPreAuthorizedTransactionId
    *   - card pre authorized transaction id
    * @return
    *   whether pre authorization transaction has been validated or not
    */
  override def validatePreAuthorization(
    orderUuid: String,
    cardPreAuthorizedTransactionId: String
  ): Boolean = {
    Try(
      MangoPay().getCardPreAuthorizationApi.get(cardPreAuthorizedTransactionId)
    ) match {
      case Success(result) =>
        result.setPaymentStatus(PaymentStatus.VALIDATED)
        Try(
          MangoPay().getCardPreAuthorizationApi.update(result)
        ) match {
          case Success(_) => true
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            false
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        false
    }
  }

  /** @param maybePayInTransaction
    *   - pay in transaction
    * @param idempotency
    *   - whether to use an idempotency key for this request or not
    * @return
    *   pay in transaction result
    */
  private[spi] def payInWithCard(
    maybePayInTransaction: Option[PayInWithCardTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] = {
    maybePayInTransaction match {
      case Some(payInTransaction) =>
        import payInTransaction._
        Option(cardId) match {
          case None =>
            mlog.error("cardId is required")
            // TODO pre registration ?
            None
          case Some(cardId) =>
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
            statementDescriptor match {
              case Some(s) =>
                paymentDetails.setStatementDescriptor(s)
              case _ =>
            }
            if (ipAddress.isDefined) {
              paymentDetails.setIpAddress(ipAddress.get)
            }
            if (browserInfo.isDefined) {
              val bi = browserInfo.get
              import bi._
              val mangoPayBrowserInfo = new MangoPayBrowserInfo()
              mangoPayBrowserInfo.setAcceptHeader(acceptHeader)
              mangoPayBrowserInfo.setColorDepth(colorDepth)
              mangoPayBrowserInfo.setJavaEnabled(javaEnabled)
              mangoPayBrowserInfo.setJavascriptEnabled(javascriptEnabled)
              mangoPayBrowserInfo.setLanguage(language)
              mangoPayBrowserInfo.setScreenHeight(screenHeight)
              mangoPayBrowserInfo.setScreenWidth(screenWidth)
              mangoPayBrowserInfo.setTimeZoneOffset(timeZoneOffset)
              mangoPayBrowserInfo.setUserAgent(userAgent)
              paymentDetails.setBrowserInfo(mangoPayBrowserInfo)
            }
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
            val idempotencyKey =
              idempotency match {
                case Some(s) if s => Some(generateUUID())
                case _            => None
              }
            Try(
              idempotencyKey match {
                case Some(s) => MangoPay().getPayInApi.create(s, payIn)
                case _       => MangoPay().getPayInApi.create(payIn)
              }
            ) match {
              case Success(result) =>
                Some(
                  Transaction().copy(
                    id = result.getId,
                    orderUuid = orderUuid,
                    nature = Transaction.TransactionNature.REGULAR,
                    `type` = Transaction.TransactionType.PAYIN,
                    status = result.getStatus match {
                      case MangoPayTransactionStatus.FAILED
                          if Option(result.getResultCode).isDefined =>
                        if (config.technicalErrors.contains(result.getResultCode))
                          Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                        else
                          Transaction.TransactionStatus.TRANSACTION_FAILED
                      case other => other
                    },
                    amount = debitedAmount,
                    cardId = Option(cardId),
                    fees = 0,
                    resultCode = Option(result.getResultCode).getOrElse(""),
                    resultMessage = Option(result.getResultMessage).getOrElse(""),
                    redirectUrl = Option( // for 3D Secure
                      result.getExecutionDetails
                        .asInstanceOf[PayInExecutionDetailsDirect]
                        .getSecureModeRedirectUrl
                    ),
                    authorId = result.getAuthorId,
                    creditedWalletId = Option(result.getCreditedWalletId),
                    idempotencyKey = idempotencyKey
                  )
                )
              case Failure(f) =>
                mlog.error(f.getMessage, f)
                None
            }
        }
      case None =>
        None
    }
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
          s"${config.payInReturnUrl}/$orderUuid?registerCard=false&printReceipt=${printReceipt.getOrElse(false)}"
        )
        payIn.setExecutionDetails(executionDetails)
        payIn.setExecutionType(PayInExecutionType.WEB)
        Try(
          MangoPay().getPayInApi.create(payIn)
        ) match {
          case Success(result) =>
            Some(
              Transaction().copy(
                id = result.getId,
                orderUuid = orderUuid,
                nature = Transaction.TransactionNature.REGULAR,
                `type` = Transaction.TransactionType.PAYIN,
                status = result.getStatus match {
                  case MangoPayTransactionStatus.FAILED if Option(result.getResultCode).isDefined =>
                    if (config.technicalErrors.contains(result.getResultCode))
                      Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                    else
                      Transaction.TransactionStatus.TRANSACTION_FAILED
                  case other => other
                },
                amount = debitedAmount,
                fees = 0,
                resultCode = Option(result.getResultCode).getOrElse(""),
                resultMessage = Option(result.getResultMessage).getOrElse(""),
                redirectUrl = Option(
                  result.getExecutionDetails
                    .asInstanceOf[PayInExecutionDetailsWeb]
                    .getRedirectUrl
                ),
                returnUrl = Option(
                  result.getExecutionDetails
                    .asInstanceOf[PayInExecutionDetailsWeb]
                    .getReturnUrl
                ),
                authorId = result.getAuthorId,
                creditedWalletId = Option(result.getCreditedWalletId),
                paymentType = Transaction.PaymentType.PAYPAL
              )
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case None => None
    }
  }

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
  ): Option[Transaction] = {
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
        val idempotencyKey =
          idempotency match {
            case Some(s) if s => Some(generateUUID())
            case _            => None
          }
        Try(
          idempotencyKey match {
            case Some(s) =>
              MangoPay().getPayInApi.createRefund(s, payInTransactionId, refund)
            case _ => MangoPay().getPayInApi.createRefund(payInTransactionId, refund)
          }
        ) match {
          case Success(result) =>
            Some(
              Transaction().copy(
                id = result.getId,
                orderUuid = orderUuid,
                nature = Transaction.TransactionNature.REFUND,
                `type` = Transaction.TransactionType.PAYIN,
                status = result.getStatus match {
                  case MangoPayTransactionStatus.FAILED if Option(result.getResultCode).isDefined =>
                    if (config.technicalErrors.contains(result.getResultCode))
                      Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                    else
                      Transaction.TransactionStatus.TRANSACTION_FAILED
                  case other => other
                },
                amount = refundAmount,
                fees = 0,
                resultCode = Option(result.getResultCode).getOrElse(""),
                resultMessage = Option(result.getResultMessage).getOrElse(""),
                reasonMessage = Option(reasonMessage),
                authorId = result.getAuthorId,
                creditedWalletId = Option(result.getCreditedWalletId),
                debitedWalletId = Option(result.getDebitedWalletId),
                idempotencyKey = idempotencyKey
              )
            )
          case Failure(f) =>
            f match {
              case r: ResponseException =>
                Some(
                  Transaction().copy(
                    id = r.getId,
                    orderUuid = orderUuid,
                    nature = Transaction.TransactionNature.REFUND,
                    `type` = Transaction.TransactionType.PAYIN,
                    status =
                      if (r.getType != "param_error") MangoPayTransactionStatus.NotSpecified
                      else MangoPayTransactionStatus.FAILED,
                    amount = refundAmount,
                    fees = 0,
                    resultCode = Option(r.getType).getOrElse(""),
                    resultMessage = Option(r.getApiMessage).getOrElse(""),
                    reasonMessage = Option(reasonMessage),
                    authorId = authorId
                  )
                )
              case _ =>
                mlog.error(f.getMessage, f)
                None
            }
        }
      case None =>
        None
    }
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
        val transfer = new Transfer()
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
        Try(MangoPay().getTransferApi.create(transfer)) match {
          case Success(result) =>
            Some(
              Transaction()
                .copy(
                  id = result.getId,
                  nature = Transaction.TransactionNature.REGULAR,
                  `type` = Transaction.TransactionType.TRANSFER,
                  status = result.getStatus match {
                    case MangoPayTransactionStatus.FAILED
                        if Option(result.getResultCode).isDefined =>
                      if (
                        config.technicalErrors
                          .contains(result.getResultCode)
                      )
                        Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                      else
                        Transaction.TransactionStatus.TRANSACTION_FAILED
                    case other => other
                  },
                  amount = debitedAmount,
                  fees = feesAmount,
                  currency = currency,
                  resultCode = Option(result.getResultCode).getOrElse(""),
                  resultMessage = Option(result.getResultMessage).getOrElse(""),
                  authorId = result.getAuthorId,
                  creditedUserId = Option(result.getCreditedUserId),
                  creditedWalletId = Some(creditedWalletId),
                  debitedWalletId = Option(result.getDebitedWalletId),
                  orderUuid = orderUuid.getOrElse(""),
                  externalReference = externalReference
                )
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            )
          case Failure(f) =>
            f match {
              case r: ResponseException =>
                Some(
                  Transaction().copy(
                    id = r.getId,
                    orderUuid = "",
                    nature = Transaction.TransactionNature.REGULAR,
                    `type` = Transaction.TransactionType.TRANSFER,
                    status =
                      if (r.getType != "param_error") MangoPayTransactionStatus.NotSpecified
                      else MangoPayTransactionStatus.FAILED,
                    amount = debitedAmount,
                    fees = feesAmount,
                    resultCode = Option(r.getType).getOrElse(""),
                    resultMessage = Option(r.getApiMessage).getOrElse(""),
                    authorId = authorId,
                    creditedUserId = Some(creditedUserId),
                    creditedWalletId = Some(creditedWalletId),
                    debitedWalletId = Some(debitedWalletId)
                  )
                )
              case _ =>
                mlog.error(f.getMessage, f)
                None
            }
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
  def payOut(
    maybePayOutTransaction: Option[PayOutTransaction],
    idempotency: Option[Boolean] = None
  ): Option[Transaction] = {
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
        val idempotencyKey =
          idempotency match {
            case Some(s) if s => Some(generateUUID())
            case _            => None
          }
        Try(
          idempotencyKey match {
            case Some(s) => MangoPay().getPayOutApi.create(s, payOut)
            case _       => MangoPay().getPayOutApi.create(payOut)
          }
        ) match {
          case Success(result) =>
            Some(
              Transaction()
                .copy(
                  id = result.getId,
                  orderUuid = orderUuid,
                  nature = Transaction.TransactionNature.REGULAR,
                  `type` = Transaction.TransactionType.PAYOUT,
                  status = result.getStatus match {
                    case MangoPayTransactionStatus.FAILED
                        if Option(result.getResultCode).isDefined =>
                      if (
                        config.technicalErrors
                          .contains(result.getResultCode)
                      )
                        Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                      else
                        Transaction.TransactionStatus.TRANSACTION_FAILED
                    case other => other
                  },
                  amount = debitedAmount,
                  fees = feesAmount,
                  currency = currency,
                  resultCode = Option(result.getResultCode).getOrElse(""),
                  resultMessage = Option(result.getResultMessage).getOrElse(""),
                  authorId = result.getAuthorId,
                  creditedUserId = Option(result.getCreditedUserId),
                  debitedWalletId = Option(result.getDebitedWalletId),
                  idempotencyKey = idempotencyKey,
                  externalReference = externalReference
                )
                .withPaymentType(Transaction.PaymentType.BANK_WIRE)
            )
          case Failure(f) =>
            f match {
              case r: ResponseException =>
                Some(
                  Transaction().copy(
                    id = r.getId,
                    orderUuid = orderUuid,
                    nature = Transaction.TransactionNature.REGULAR,
                    `type` = Transaction.TransactionType.PAYOUT,
                    status =
                      if (r.getType != "param_error") MangoPayTransactionStatus.NotSpecified
                      else MangoPayTransactionStatus.FAILED,
                    amount = debitedAmount,
                    fees = feesAmount,
                    resultCode = Option(r.getType).getOrElse(""),
                    resultMessage = Option(r.getApiMessage).getOrElse(""),
                    authorId = authorId,
                    creditedUserId = Some(creditedUserId),
                    debitedWalletId = Some(debitedWalletId),
                    externalReference = externalReference
                  )
                )
              case _ =>
                mlog.error(f.getMessage, f)
                None
            }
        }
      case None =>
        None
    }
  }

  /** @param orderUuid
    *   - order unique id
    * @param transactionId
    *   - transaction id
    * @return
    *   pay in transaction
    */
  def loadPayInTransaction(
    orderUuid: String,
    transactionId: String,
    recurringPayInRegistrationId: Option[String]
  ): Option[Transaction] = {
    Try(MangoPay().getPayInApi.get(transactionId)) match {
      case Success(result) =>
        val `type` =
          if (result.getPaymentType == PayInPaymentType.DIRECT_DEBIT) {
            Transaction.TransactionType.DIRECT_DEBIT
          } else {
            Transaction.TransactionType.PAYIN
          }
        val cardId =
          if (result.getPaymentType == PayInPaymentType.CARD) {
            Option(result.getPaymentDetails.asInstanceOf[PayInPaymentDetailsCard].getCardId)
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
            id = result.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = `type`,
            status = result.getStatus,
            amount = result.getDebitedFunds.getAmount,
            cardId = cardId,
            fees = result.getFees.getAmount,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
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
      case Failure(_) =>
        None
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
    Try(MangoPay().getPayInApi.getRefund(transactionId)) match {
      case Success(result) =>
        Some(
          Transaction().copy(
            id = result.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REFUND,
            `type` = Transaction.TransactionType.PAYIN,
            status = result.getStatus,
            amount = result.getDebitedFunds.getAmount,
            fees = 0,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
            reasonMessage = Option(result.getRefundReason.getRefundReasonMessage),
            authorId = result.getAuthorId,
            creditedWalletId = Option(result.getCreditedWalletId),
            debitedWalletId = Option(result.getDebitedWalletId)
          )
        )
      case Failure(_) =>
        None
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
    Try(MangoPay().getPayOutApi.get(transactionId)) match {
      case Success(result) =>
        Some(
          Transaction().copy(
            id = result.getId,
            orderUuid = orderUuid,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.PAYOUT,
            status = result.getStatus,
            amount = result.getDebitedFunds.getAmount,
            fees = result.getFees.getAmount,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
            authorId = result.getAuthorId,
            creditedUserId = Option(result.getCreditedUserId),
            debitedWalletId = Option(result.getDebitedWalletId)
          )
        )
      case Failure(_) =>
        None
    }
  }

  /** @param transactionId
    *   - transaction id
    * @return
    *   transfer transaction
    */
  override def loadTransfer(transactionId: String): Option[Transaction] = {
    Try(MangoPay().getTransferApi.get(transactionId)) match {
      case Success(result) =>
        Some(
          Transaction().copy(
            id = result.getId,
            nature = Transaction.TransactionNature.REGULAR,
            `type` = Transaction.TransactionType.TRANSFER,
            status = result.getStatus,
            amount = result.getDebitedFunds.getAmount,
            fees = result.getFees.getAmount,
            resultCode = Option(result.getResultCode).getOrElse(""),
            resultMessage = Option(result.getResultMessage).getOrElse(""),
            authorId = result.getAuthorId,
            creditedUserId = Option(result.getCreditedUserId),
            creditedWalletId = Option(result.getCreditedWalletId),
            debitedWalletId = Option(result.getDebitedWalletId)
          )
        )
      case Failure(_) => None
    }
  }

  protected[spi] def checkEquality(
    bankAccount1: MangoPayBankAccount,
    bankAccount2: MangoPayBankAccount
  ): Boolean = {
    if (
      bankAccount1.getType == BankAccountType.IBAN
      && bankAccount1.getType == bankAccount2.getType
      && bankAccount1.getUserId == bankAccount2.getUserId
      && bankAccount1.getOwnerName == bankAccount2.getOwnerName
      && bankAccount1.getOwnerAddress.getCountry == bankAccount2.getOwnerAddress.getCountry
      && bankAccount1.getOwnerAddress.getCity == bankAccount2.getOwnerAddress.getCity
      && bankAccount1.getOwnerAddress.getPostalCode == bankAccount2.getOwnerAddress.getPostalCode
      && bankAccount1.getOwnerAddress.getAddressLine1 == bankAccount2.getOwnerAddress.getAddressLine1
    ) {
      val details1 = bankAccount1.getDetails.asInstanceOf[BankAccountDetailsIBAN]
      val details2 = bankAccount2.getDetails.asInstanceOf[BankAccountDetailsIBAN]
      details1.getBic == details2.getBic && details1.getIban == details2.getIban
    } else {
      false
    }
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
  def addDocument(
    userId: String,
    externalUuid: String,
    pages: Seq[Array[Byte]],
    documentType: KycDocument.KycDocumentType
  ): Option[String] = {
    // create document
    Try(MangoPay().getUserApi.createKycDocument(userId, documentType, externalUuid)) match {
      case Success(s) =>
        mlog.info(s"""Create $documentType for $externalUuid""")
        // ask for document creation
        s.setTag(externalUuid)
        s.setStatus(KycStatus.CREATED)
        Try(MangoPay().getUserApi.updateKycDocument(userId, s)) match {
          case Success(s2) =>
            mlog.info(s"""Update $documentType for $externalUuid""")
            // add document pages
            if (
              pages.forall { page =>
                Try(MangoPay().getUserApi.createKycPage(userId, s2.getId, page)) match {
                  case Success(_) =>
                    mlog.info(s"""Add document page for $externalUuid""")
                    true
                  case Failure(f3) =>
                    mlog.error(f3.getMessage, f3.getCause)
                    false
                }
              }
            ) {
              // ask for document validation
              s2.setTag(externalUuid)
              s2.setStatus(KycStatus.VALIDATION_ASKED)
              Try(MangoPay().getUserApi.updateKycDocument(userId, s2)) match {
                case Success(s3) =>
                  mlog.info(s"""Ask document ${s3.getId} validation for $externalUuid""")
                  Some(s3.getId)
                case Failure(f3) =>
                  mlog.error(f3.getMessage, f3.getCause)
                  None
              }
            } else {
              None
            }
          case Failure(f2) =>
            mlog.error(f2.getMessage, f2.getCause)
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f.getCause)
        None
    }
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
    // load document
    Try(MangoPay().getUserApi.getKycDocument(userId, documentId)) match {
      case Success(s) =>
        KycDocumentValidationReport.defaultInstance
          .withId(documentId)
          .withType(s.getType)
          .withStatus(s.getStatus)
          .copy(
            tag = Option(s.getTag),
            refusedReasonType = Option(s.getRefusedReasonType),
            refusedReasonMessage = Option(s.getRefusedReasonMessage)
          )
      case Failure(f) =>
        mlog.error(f.getMessage, f.getCause)
        KycDocumentValidationReport.defaultInstance
          .withId(documentId)
          .withStatus(KycDocument.KycDocumentStatus.KYC_DOCUMENT_NOT_SPECIFIED)
    }

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
    mandate.setBankAccountId(bankAccountId)
    mandate.setCulture(CultureCode.FR)
    mandate.setExecutionType(MandateExecutionType.WEB)
    mandate.setMandateType(MandateType.DIRECT_DEBIT)
    mandate.setReturnUrl(
      s"${config.mandateReturnUrl}?externalUuid=$externalUuid&idempotencyKey=${idempotencyKey.getOrElse("")}"
    )
    mandate.setScheme(MandateScheme.SEPA)
    mandate.setUserId(userId)
    Try(
      idempotencyKey match {
        case Some(key) => MangoPay().getMandateApi.create(key, mandate)
        case _         => MangoPay().getMandateApi.create(mandate)
      }
    ) match {
      case Success(s) =>
        if (s.getStatus.isMandateFailed) {
          mlog.error(
            s"mandate creation failed for $externalUuid -> (${s.getResultCode}, ${s.getResultMessage}"
          )
        }
        Some(
          MandateResult.defaultInstance
            .withId(s.getId)
            .withStatus(s.getStatus)
            .withRedirectUrl(s.getRedirectUrl)
            .copy(
              resultCode = Option(s.getResultCode),
              resultMessage = Option(s.getResultMessage)
            )
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param maybeMandateId
    *   - optional mandate id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - bank account id
    * @return
    *   active or submitted mandate associated to this bank account
    */
  override def loadMandate(
    maybeMandateId: Option[String],
    userId: String,
    bankAccountId: String
  ): Option[MandateResult] = {
    Try(
      maybeMandateId match {
        case Some(mandateId) => Option(MangoPay().getMandateApi.get(mandateId))
        case None =>
          val sorting = new Sorting()
          sorting.addField("creationDate", SortDirection.desc)
          MangoPay().getMandateApi
            .getForBankAccount(
              userId,
              bankAccountId,
              new FilterMandates(),
              new Pagination(1, 100),
              sorting
            )
            .asScala
            .toList
            .find(mandate =>
              mandate.getStatus.isMandateActivated || mandate.getStatus.isMandateSubmitted
            )
      }
    ) match {
      case Success(s) =>
        s match {
          case Some(mandate)
              if mandate.getBankAccountId == bankAccountId && mandate.getUserId == userId =>
            Some(
              MandateResult.defaultInstance
                .withId(mandate.getId)
                .withStatus(mandate.getStatus)
                .withRedirectUrl(mandate.getRedirectUrl)
                .copy(
                  resultCode = Option(mandate.getResultCode),
                  resultMessage = Option(mandate.getResultMessage)
                )
            )
          case _ => None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param mandateId
    *   - Provider mandate id
    * @return
    *   mandate result
    */
  override def cancelMandate(mandateId: String): Option[MandateResult] = {
    Try(MangoPay().getMandateApi.cancel(mandateId)) match {
      case Success(mandate) =>
        Some(
          MandateResult.defaultInstance
            .withId(mandate.getId)
            .withStatus(mandate.getStatus)
            .withRedirectUrl(mandate.getRedirectUrl)
            .copy(
              resultCode = Option(mandate.getResultCode),
              resultMessage = Option(mandate.getResultMessage)
            )
        )
      case Failure(f) =>
        f match {
          case r: ResponseException if r.getType == "ressource_not_found" =>
            Some(
              MandateResult.defaultInstance
                .withId(mandateId)
                .withStatus(MandateStatus.FAILED)
                .withRedirectUrl("")
                .copy(
                  resultCode = Some(r.getType),
                  resultMessage = Some(r.getMessage)
                )
            )
          case _ =>
            mlog.error(f.getMessage, f)
            None
        }
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
        val idempotencyKey =
          idempotency match {
            case Some(s) if s => Some(generateUUID())
            case _            => None
          }
        Try(
          idempotencyKey match {
            case Some(s) => MangoPay().getPayInApi.create(s, payIn)
            case _       => MangoPay().getPayInApi.create(payIn)
          }
        ) match {
          case Success(result) =>
            Some(
              Transaction()
                .copy(
                  id = result.getId,
                  nature = Transaction.TransactionNature.REGULAR,
                  `type` = Transaction.TransactionType.DIRECT_DEBIT,
                  status = result.getStatus match {
                    case MangoPayTransactionStatus.FAILED
                        if Option(result.getResultCode).isDefined =>
                      if (
                        config.technicalErrors
                          .contains(result.getResultCode)
                      )
                        Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                      else
                        Transaction.TransactionStatus.TRANSACTION_FAILED
                    case other => other
                  },
                  amount = debitedAmount,
                  fees = feesAmount,
                  resultCode = Option(result.getResultCode).getOrElse(""),
                  resultMessage = Option(result.getResultMessage).getOrElse(""),
                  redirectUrl = Option( // for 3D Secure
                    result.getExecutionDetails
                      .asInstanceOf[PayInExecutionDetailsDirect]
                      .getSecureModeRedirectUrl
                  ),
                  authorId = result.getAuthorId,
                  creditedUserId = Option(result.getCreditedUserId),
                  creditedWalletId = Option(result.getCreditedWalletId),
                  mandateId = Some(mandateId),
                  externalReference = externalReference,
                  idempotencyKey = idempotencyKey
                )
                .withPaymentType(Transaction.PaymentType.DIRECT_DEBITED)
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case None => None
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
    val sorting = new Sorting()
    sorting.addField("creationDate", SortDirection.desc)
    val filters = new FilterTransactions()
    filters.setNature(MangoPayTransactionNature.REGULAR)
    filters.setType(MangoPayTransactionType.PAYIN)
    filters.setAfterDate(transactionDate.toEpochSecond)
    Try(
      MangoPay().getWalletApi
        .getTransactions(
          walletId,
          new Pagination(1, 100),
          filters,
          sorting
        )
        .asScala
        .find(_.getId == transactionId)
    ) match {
      case Success(s) =>
        s match {
          case Some(result) =>
            Some(
              Transaction().copy(
                id = result.getId,
                nature = Transaction.TransactionNature.REGULAR,
                `type` = Transaction.TransactionType.PAYIN,
                status = result.getStatus match {
                  case MangoPayTransactionStatus.FAILED if Option(result.getResultCode).isDefined =>
                    if (config.technicalErrors.contains(result.getResultCode))
                      Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                    else
                      Transaction.TransactionStatus.TRANSACTION_FAILED
                  case other => other
                },
                amount = result.getDebitedFunds.getAmount,
                fees = result.getFees.getAmount,
                resultCode = Option(result.getResultCode).getOrElse(""),
                resultMessage = Option(result.getResultMessage).getOrElse(""),
                authorId = result.getAuthorId,
                creditedUserId = Option(result.getCreditedUserId),
                creditedWalletId = Some(walletId)
              )
            )
          case _ => None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  override def client: Option[SoftPayAccount.Client] = {
    Try(MangoPay().getClientApi.get()) match {
      case Success(client) =>
        Some(
          SoftPayAccount.Client.defaultInstance
            .withClientId(client.getClientId + "." + provider.providerType.name.toLowerCase)
            .withProvider(provider)
            .copy(
              name = Option(client.getName),
              description = Option(client.getPlatformDescription),
              logoUrl = Option(client.getLogo),
              websiteUrl = Option(client.getPlatformUrl),
              address = Option(client.getHeadquartersAddress).map(address =>
                Address.defaultInstance
                  .withAddressLine(address.getAddressLine1)
                  .withCity(address.getCity)
                  .withPostalCode(address.getPostalCode)
                  .withCountry(address.getCountry.name())
                  .copy(state = Option(address.getRegion))
              ),
              technicalEmails = client.getTechEmails.asScala,
              administrativeEmails = client.getAdminEmails.asScala,
              billingEmails = client.getBillingEmails.asScala,
              fraudEmails = client.getFraudEmails.asScala,
              vatNumber = Option(client.getTaxNumber)
            )
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @return
    *   client fees
    */
  override def clientFees(): Option[Double] = {
    Try(MangoPay().getClientApi.getWallet(FundsType.FEES, CurrencyIso.EUR)) match {
      case Success(wallet) => Some(wallet.getBalance.getAmount.toDouble / 100)
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param userId
    *   - Provider user id
    * @return
    *   Ultimate Beneficial Owner Declaration
    */
  override def createDeclaration(userId: String): Option[UboDeclaration] = {
    Try(MangoPay().getUboDeclarationApi.create(userId)) match {
      case Success(declaration) =>
        Some(
          UboDeclaration.defaultInstance
            .withId(declaration.getId)
            .withStatus(declaration.getStatus)
            .withCreatedDate(declaration.getCreationDate)
            .copy(
              reason = Option(declaration.getReason),
              message = Option(declaration.getMessage)
            )
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
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
    import ultimateBeneficialOwner._
    val sdf = new SimpleDateFormat("dd/MM/yyyy")
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    Try(sdf.parse(birthday)) match {
      case Success(s) =>
        val ubo = new Ubo
        ubo.setId(id.getOrElse(""))
        ubo.setFirstName(firstName)
        ubo.setLastName(lastName)
        ubo.setNationality(CountryIso.valueOf(nationality))
        ubo.setBirthday(s.toEpochSecond)
        val a = new MangoPayAddress
        a.setAddressLine1(address)
        a.setCity(city)
        a.setPostalCode(postalCode)
        a.setRegion(region)
        a.setCountry(CountryIso.valueOf(country))
        ubo.setAddress(a)

        val b = new MangoPayBirthplace
        b.setCity(birthPlace.city)
        b.setCountry(CountryIso.valueOf(birthPlace.country))
        ubo.setBirthplace(b)
        ubo.setActive(active)

        {
          if (id.isEmpty) {
            Try(MangoPay().getUboDeclarationApi.createUbo(userId, uboDeclarationId, ubo))
          } else {
            Try(MangoPay().getUboDeclarationApi.updateUbo(userId, uboDeclarationId, ubo))
          }
        } match {
          case Success(s2) =>
            Some(ultimateBeneficialOwner.copy(id = Option(s2.getId)))
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  /** @param userId
    *   - Provider user id
    * @param uboDeclarationId
    *   - Provider declaration id
    * @return
    *   declaration with Ultimate Beneficial Owner(s)
    */
  override def getDeclaration(userId: String, uboDeclarationId: String): Option[UboDeclaration] = {
    Try(MangoPay().getUboDeclarationApi.get(userId, uboDeclarationId)) match {
      case Success(s) =>
        import scala.collection.JavaConverters._
        Some(
          UboDeclaration.defaultInstance
            .withId(uboDeclarationId)
            .withStatus(s.getStatus)
            .withCreatedDate(s.getCreationDate)
            .copy(
              reason = Option(s.getReason),
              message = Option(s.getMessage)
            )
            .withUbos(s.getUbos.asScala.map(ubo => {
              UboDeclaration.UltimateBeneficialOwner.defaultInstance
                .withId(ubo.getId)
                .withFirstName(ubo.getFirstName)
                .withLastName(ubo.getLastName)
                .withBirthday(computeBirthday(ubo.getBirthday))
                .withNationality(ubo.getNationality.name())
                .withAddress(ubo.getAddress.getAddressLine1)
                .withCity(ubo.getAddress.getCity)
                .withPostalCode(ubo.getAddress.getPostalCode)
                .withRegion(ubo.getAddress.getRegion)
                .withCountry(ubo.getAddress.getCountry.name())
                .withActive(ubo.getActive)
                .withBirthPlace(
                  UboDeclaration.UltimateBeneficialOwner.BirthPlace.defaultInstance
                    .withCity(ubo.getBirthplace.getCity)
                    .withCountry(ubo.getBirthplace.getCountry.name())
                )
            }))
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

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
    Try(
      MangoPay().getUboDeclarationApi.submitForValidation(userId, uboDeclarationId)
    ) match {
      case Success(s) =>
        Some(
          UboDeclaration.defaultInstance
            .withId(uboDeclarationId)
            .withStatus(s.getStatus)
            .withCreatedDate(s.getCreationDate)
            .copy(
              reason = Option(s.getReason),
              message = Option(s.getMessage)
            )
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
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
      Try(
        MangoPay().getPayInApi
          .createRecurringPayment(generateUUID(), createRecurringPayment)
      ) match {
        case Success(s) =>
          Some(
            RecurringPayment.RecurringCardPaymentResult.defaultInstance
              .withId(s.getId)
              .withStatus(s.getStatus)
          )
        case Failure(f) =>
          mlog.error(f.getMessage, f)
          None
      }
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
      val recurringPaymentUpdate = new RecurringPaymentUpdate
      cardId match {
        case Some(s) => recurringPaymentUpdate.setCardId(s)
        case _       =>
      }
      recurringPaymentUpdate.setCardId(cardId.getOrElse(""))
      status match {
        case Some(s) => recurringPaymentUpdate.setStatus(s.name)
        case _       =>
      }
      Try(
        MangoPay().getPayInApi
          .updateRecurringPayment(recurringPayInRegistrationId, recurringPaymentUpdate)
      ) match {
        case Success(s) =>
          Some(
            RecurringPayment.RecurringCardPaymentResult.defaultInstance
              .withId(recurringPayInRegistrationId)
              .withStatus(s.getStatus)
              .withCurrentstate(s.getCurrentState)
          )
        case Failure(f) =>
          mlog.error(f.getMessage, f)
          None
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
    Try(
      MangoPay().getPayInApi.getRecurringPayment(recurringPayInRegistrationId)
    ) match {
      case Success(s) =>
        Some(
          RecurringPayment.RecurringCardPaymentResult.defaultInstance
            .withId(recurringPayInRegistrationId)
            .withStatus(s.getStatus)
            .withCurrentstate(s.getCurrentState)
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
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
    recurringPaymentTransaction.extension[Option[FirstRecurringPaymentTransaction]](
      FirstRecurringPaymentTransaction.first
    ) match {
      case Some(firstRecurringPaymentTransaction) =>
        import recurringPaymentTransaction._
        val recurringPayInCIT: RecurringPayInCIT = new RecurringPayInCIT
        recurringPayInCIT.setRecurringPayInRegistrationId(recurringPaymentRegistrationId)
        firstRecurringPaymentTransaction.ipAddress match {
          case Some(ipAddress) => recurringPayInCIT.setIpAddress(ipAddress)
          case _               =>
        }
        firstRecurringPaymentTransaction.browserInfo match {
          case Some(browserInfo) =>
            import browserInfo._
            val mangoPayBrowserInfo = new MangoPayBrowserInfo()
            mangoPayBrowserInfo.setAcceptHeader(acceptHeader)
            mangoPayBrowserInfo.setColorDepth(colorDepth)
            mangoPayBrowserInfo.setJavaEnabled(javaEnabled)
            mangoPayBrowserInfo.setJavascriptEnabled(javascriptEnabled)
            mangoPayBrowserInfo.setLanguage(language)
            mangoPayBrowserInfo.setScreenHeight(screenHeight)
            mangoPayBrowserInfo.setScreenWidth(screenWidth)
            mangoPayBrowserInfo.setTimeZoneOffset(timeZoneOffset)
            mangoPayBrowserInfo.setUserAgent(userAgent)
            recurringPayInCIT.setBrowserInfo(mangoPayBrowserInfo)
          case _ =>
        }
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
          s"${config.recurringPaymentReturnUrl}/$recurringPaymentRegistrationId"
        )
        Try(
          MangoPay().getPayInApi.createRecurringPayInCIT(generateUUID(), recurringPayInCIT)
        ) match {
          case Success(result) =>
            Some(
              Transaction().copy(
                id = result.getId,
                nature = Transaction.TransactionNature.REGULAR,
                `type` = Transaction.TransactionType.PAYIN,
                status = result.getStatus match {
                  case MangoPayTransactionStatus.FAILED if Option(result.getResultCode).isDefined =>
                    if (config.technicalErrors.contains(result.getResultCode))
                      Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                    else
                      Transaction.TransactionStatus.TRANSACTION_FAILED
                  case other => other
                },
                amount = debitedAmount,
                cardId =
                  Option(result.getPaymentDetails.asInstanceOf[PayInPaymentDetailsCard].getCardId),
                fees = feesAmount,
                resultCode = Option(result.getResultCode).getOrElse(""),
                resultMessage = Option(result.getResultMessage).getOrElse(""),
                redirectUrl = Option( // for 3D Secure
                  result.getExecutionDetails
                    .asInstanceOf[PayInExecutionDetailsDirect]
                    .getSecureModeRedirectUrl
                ),
                authorId = result.getAuthorId,
                creditedWalletId = Option(result.getCreditedWalletId),
                recurringPayInRegistrationId = Option(recurringPaymentRegistrationId)
              )
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ =>
        import recurringPaymentTransaction._
        val recurringPayInMIT: RecurringPayInMIT = new RecurringPayInMIT
        recurringPayInMIT.setRecurringPayInRegistrationId(recurringPaymentRegistrationId)
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
        Try(
          MangoPay().getPayInApi.createRecurringPayInMIT(generateUUID(), recurringPayInMIT)
        ) match {
          case Success(result) =>
            Some(
              Transaction().copy(
                id = result.getId,
                nature = Transaction.TransactionNature.REGULAR,
                `type` = Transaction.TransactionType.PAYIN,
                status = result.getStatus match {
                  case MangoPayTransactionStatus.FAILED if Option(result.getResultCode).isDefined =>
                    if (config.technicalErrors.contains(result.getResultCode))
                      Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
                    else
                      Transaction.TransactionStatus.TRANSACTION_FAILED
                  case other => other
                },
                amount = debitedAmount,
                cardId =
                  Option(result.getPaymentDetails.asInstanceOf[PayInPaymentDetailsCard].getCardId),
                fees = feesAmount,
                resultCode = Option(result.getResultCode).getOrElse(""),
                resultMessage = Option(result.getResultMessage).getOrElse(""),
                authorId = result.getAuthorId,
                creditedWalletId = Option(result.getCreditedWalletId),
                recurringPayInRegistrationId = Option(recurringPaymentRegistrationId)
              )
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
    }
  }

}

class MangoPayProviderFactory extends PaymentProviderSpi {
  @volatile private[this] var _config: Option[MangoPayConfig] = None

  override val providerType: Provider.ProviderType =
    Provider.ProviderType.MANGOPAY

  override def paymentProvider(p: SoftPayAccount.Client.Provider): MangoPayProvider =
    new MangoPayProvider {
      override implicit val provider: SoftPayAccount.Client.Provider = p
      override implicit val config: MangoPayConfig =
        _config.getOrElse(MangoPaySettings.MangoPayConfig)
    }

  override def softPaymentProvider(config: Config): SoftPayAccount.Client.Provider = {
    val mangoPayConfig = MangoPaySettings(config).MangoPayConfig
    _config = Some(mangoPayConfig)
    mangoPayConfig.softPayProvider
  }

  override def hooksDirectives(implicit
    _system: ActorSystem[_],
    _formats: Formats
  ): HooksDirectives =
    new MangoPayHooksDirectives {
      override implicit def system: ActorSystem[_] = _system
      override implicit def formats: Formats = _formats
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
    }

  override def hooksEndpoints(implicit _system: ActorSystem[_], formats: Formats): HooksEndpoints =
    new MangoPayHooksEndpoints {
      override implicit def system: ActorSystem[_] = _system
      override def log: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass)
    }
}
