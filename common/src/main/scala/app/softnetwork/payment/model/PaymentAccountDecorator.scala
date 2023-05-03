package app.softnetwork.payment.model

import app.softnetwork.persistence._

import java.time.Instant

trait PaymentAccountDecorator { self: PaymentAccount =>

  lazy val maybeUser: Option[PaymentUser] = {
    if (user.isLegalUser) {
      Some(getLegalUser.legalRepresentative)
    } else if (user.isNaturalUser) {
      Some(getNaturalUser)
    } else {
      None
    }
  }

  lazy val externalUuid: String = maybeUser match {
    case Some(user) => user.externalUuid
    case _          => generateUUID()
  }

  lazy val profile: Option[String] = maybeUser.flatMap(_.profile)

  lazy val externalUuidWithProfile: String = {
    maybeUser match {
      case Some(user) => user.externalUuidWithProfile
      case _          => externalUuid
    }
  }

  lazy val userId: Option[String] = maybeUser.flatMap(_.userId)

  lazy val walletId: Option[String] = maybeUser.flatMap(_.walletId)

  lazy val email: Option[String] = maybeUser.map(_.email)

  lazy val emptyUser: Boolean = user.isEmpty

  lazy val legalUser: Boolean = user.isLegalUser

  lazy val legalUserType: Option[LegalUser.LegalUserType] = user.legalUser.map(_.legalUserType)

  def checkIfSameLegalUserType(newlegalUserType: Option[LegalUser.LegalUserType]): Boolean =
    legalUserType.getOrElse(LegalUser.LegalUserType.Unrecognized(-1)) ==
      newlegalUserType.getOrElse(LegalUser.LegalUserType.Unrecognized(-1))

  lazy val documentsValidated: Boolean = documents.forall(_.status.isKycDocumentValidated)

  lazy val documentOutdated: Boolean = documents.exists(_.status.isKycDocumentOutOfDate)

  lazy val mandateExists: Boolean = bankAccount.flatMap(_.mandateId).isDefined &&
    bankAccount
      .flatMap(_.mandateStatus)
      .exists(s => s.isMandateCreated || s.isMandateActivated || s.isMandateSubmitted)

  lazy val mandateRequired: Boolean =
    recurryingPayments.exists(r => r.`type`.isDirectDebit && r.nextPaymentDate.isDefined) ||
    transactions.exists(t => t.paymentType.isDirectDebited && t.status.isTransactionCreated)

  lazy val mandateActivated: Boolean = bankAccount.flatMap(_.mandateId).isDefined &&
    bankAccount.flatMap(_.mandateStatus).exists(s => s.isMandateActivated || s.isMandateSubmitted)

  def resetUserId(userId: Option[String] = None): PaymentAccount = {
    val updatedBankAccount = bankAccount match {
      case Some(s) => Some(s.withUserId(userId.getOrElse("")))
      case _       => None
    }
    if (user.isLegalUser) {
      val user = getLegalUser
      self
        .withLegalUser(user.withLegalRepresentative(user.legalRepresentative.copy(userId = userId)))
        .copy(
          bankAccount = updatedBankAccount
        )
    } else if (user.isNaturalUser) {
      val user = getNaturalUser
      self
        .withNaturalUser(user.copy(userId = userId))
        .copy(
          bankAccount = updatedBankAccount
        )
    } else {
      self.copy(
        bankAccount = updatedBankAccount
      )
    }
  }

  def resetWalletId(walletId: Option[String] = None): PaymentAccount = {
    if (user.isLegalUser) {
      val user = getLegalUser
      self.withLegalUser(
        user.withLegalRepresentative(user.legalRepresentative.copy(walletId = walletId))
      )
    } else if (user.isNaturalUser) {
      val user = getNaturalUser
      self.withNaturalUser(user.copy(walletId = walletId))
    } else {
      self
    }
  }

  def resetBankAccountId(id: Option[String] = None): PaymentAccount = {
    self.copy(bankAccount = self.bankAccount.map(_.copy(id = id)))
  }

  lazy val hasAcceptedTermsOfPSP: Boolean =
    !legalUser || getLegalUser.lastAcceptedTermsOfPSP.isDefined

  lazy val view: PaymentAccountView = PaymentAccountView(self)
}

case class PaymentAccountView(
  createdDate: Instant,
  lastUpdated: Instant,
  naturalUser: Option[PaymentUserView] = None,
  legalUser: Option[LegalUserView] = None,
  cards: Seq[CardView] = Seq.empty,
  bankAccount: Option[BankAccountView] = None,
  documents: Seq[KycDocumentView] = Seq.empty,
  paymentAccountStatus: PaymentAccount.PaymentAccountStatus,
  transactions: Seq[TransactionView] = Seq.empty
)

object PaymentAccountView {
  def apply(paymentAccount: PaymentAccount): PaymentAccountView = {
    import paymentAccount._
    PaymentAccountView(
      createdDate,
      lastUpdated,
      if (user.isNaturalUser) {
        Option(getNaturalUser.view)
      } else {
        None
      },
      if (user.isLegalUser) {
        Option(getLegalUser.view)
      } else {
        None
      },
      cards.map(_.view),
      bankAccount.map(_.view),
      documents.map(_.view),
      paymentAccountStatus,
      transactions.map(_.view)
    )
  }
}
