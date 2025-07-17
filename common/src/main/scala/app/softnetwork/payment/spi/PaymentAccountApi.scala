package app.softnetwork.payment.spi

import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.model.{
  BankAccount,
  KycDocument,
  KycDocumentValidationReport,
  LegalUser,
  NaturalUser,
  PaymentAccount,
  UboDeclaration
}

trait PaymentAccountApi { _: PaymentContext =>

  /** @param maybePaymentAccount
    *   - payment account to create or update
    * @param acceptedTermsOfPSP
    *   - whether the user has accepted the terms of the PSP
    * @param ipAddress
    *   - ip address of the user
    * @param userAgent
    *   - user agent of the user
    * @param tokenId
    *   - optional token id for the payment account
    * @return
    *   provider user id
    */
  def createOrUpdatePaymentAccount(
    maybePaymentAccount: Option[PaymentAccount],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String],
    tokenId: Option[String] = None
  ): Option[String] = {
    maybePaymentAccount match {
      case Some(paymentAccount) =>
        import paymentAccount._
        if (user.isLegalUser) {
          createOrUpdateLegalUser(user.legalUser, acceptedTermsOfPSP, ipAddress, userAgent, tokenId)
        } else if (user.isNaturalUser) {
          createOrUpdateNaturalUser(
            user.naturalUser,
            acceptedTermsOfPSP,
            ipAddress,
            userAgent,
            tokenId
          )
        } else {
          None
        }
      case _ => None
    }
  }

  /** @param maybeNaturalUser
    *   - natural user to create
    * @param acceptedTermsOfPSP
    *   - whether the user has accepted the terms of the PSP
    * @param ipAddress
    *   - ip address of the user
    * @param userAgent
    *   - user agent of the user
    * @param tokenId
    *   - optional token id for the payment account
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] def createOrUpdateNaturalUser(
    maybeNaturalUser: Option[NaturalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String],
    tokenId: Option[String]
  ): Option[String]

  /** @param maybeLegalUser
    *   - legal user to create or update
    * @param acceptedTermsOfPSP
    *   - whether the user has accepted the terms of the PSP
    * @param ipAddress
    *   - ip address of the user
    * @param userAgent
    *   - user agent of the user
    * @param tokenId
    *   - optional token id for the payment account
    * @return
    *   provider user id
    */
  @InternalApi
  private[spi] def createOrUpdateLegalUser(
    maybeLegalUser: Option[LegalUser],
    acceptedTermsOfPSP: Boolean,
    ipAddress: Option[String],
    userAgent: Option[String],
    tokenId: Option[String]
  ): Option[String]

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
    * @param ipAddress
    *   - ip address of the user
    * @param userAgent
    *   - user agent of the user
    * @param tokenId
    *   - optional token id for the payment account
    * @return
    *   Ultimate Beneficial Owner declaration
    */
  def validateDeclaration(
    userId: String,
    uboDeclarationId: String,
    ipAddress: String,
    userAgent: String,
    tokenId: Option[String]
  ): Option[UboDeclaration]

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
    * @param documentType
    *   - document type
    * @return
    *   document validation report
    */
  def loadDocumentStatus(
    userId: String,
    documentId: String,
    documentType: KycDocument.KycDocumentType
  ): KycDocumentValidationReport

  /** @param maybeBankAccount
    *   - bank account to create
    * @param bankTokenId
    *   - optional bank token id for the payment account
    * @return
    *   bank account id
    */
  def createOrUpdateBankAccount(
    maybeBankAccount: Option[BankAccount],
    bankTokenId: Option[String]
  ): Option[String]

  /** @param userId
    *   - provider user id
    * @param currency
    *   - currency
    * @return
    *   the first active bank account
    */
  def getActiveBankAccount(userId: String, currency: String): Option[String]

  /** @param userId
    *   - provider user id
    * @param bankAccountId
    *   - bank account id
    * @param currency
    *   - currency
    * @return
    *   whether this bank account exists and is active
    */
  def checkBankAccount(userId: String, bankAccountId: String, currency: String): Boolean = {
    getActiveBankAccount(userId, currency).contains(bankAccountId)
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
  ): Option[String]

}
