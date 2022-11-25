package app.softnetwork.payment.model

import app.softnetwork.security._
import app.softnetwork.validation.RegexValidator
import org.apache.commons.validator.routines.IBANValidator

import scala.util.matching.Regex

trait BankAccountDecorator { self: BankAccount =>
  lazy val wrongIban: Boolean = !IBANValidator.getInstance.isValid(iban)

  lazy val wrongBic: Boolean = !BicValidator.check(bic)

  lazy val wrongOwnerName: Boolean = !NameValidator.check(ownerName)

  lazy val wrongOwnerAddress: Boolean = ownerAddress.wrongAddress

  def validate(): Boolean = !wrongIban && !wrongBic && !wrongOwnerName && !wrongOwnerAddress

  def encode(): BankAccount = {
    if (!encoded) {
      this
        .withBic(sha256(bic))
        .withIban(sha256(iban))
        .withEncoded(true)
    } else {
      this
    }
  }

  def checkIfSameIban(newIban: String): Boolean = {
    if (encoded) {
      iban == sha256(newIban)
    } else {
      iban == newIban
    }
  }

  def checkIfSameBic(newBic: String): Boolean = {
    if (encoded) {
      bic == sha256(newBic)
    } else {
      bic == newBic
    }
  }

  lazy val tag: String = externalUuid

  lazy val view: BankAccountView = BankAccountView(self)
}

case class BankAccountView(
  createdDate: java.util.Date,
  lastUpdated: java.util.Date,
  bankAccountId: Option[String] = None,
  ownerName: String,
  ownerAddress: AddressView,
  iban: String,
  bic: String,
  encoded: Boolean,
  active: Boolean,
  mandateId: Option[String] = None,
  mandateStatus: Option[BankAccount.MandateStatus] = None,
  externalUuid: String
)

object BankAccountView {
  def apply(bankAccount: BankAccount): BankAccountView = {
    val encodedBankAccount =
      if (bankAccount.encoded) {
        bankAccount
      } else {
        bankAccount.encode()
      }
    import encodedBankAccount._
    BankAccountView(
      createdDate,
      lastUpdated,
      id,
      ownerName,
      ownerAddress.view,
      iban,
      bic,
      encoded,
      active,
      mandateId,
      mandateStatus,
      externalUuid
    )
  }
}

object BicValidator extends RegexValidator {
  val regex: Regex = """^[a-zA-Z]{6}\w{2}(\w{3})?$""".r
}

object NameValidator extends RegexValidator {
  val regex: Regex = """^[a-zA-Z]{1}[a-zA-Z\séèêïî]{0,24}""".r
}
