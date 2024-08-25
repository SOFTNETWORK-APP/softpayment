package app.softnetwork.payment

import app.softnetwork.payment.model._
import app.softnetwork.protobuf.ScalaPBSerializers
import ScalaPBSerializers.GeneratedEnumSerializer
import app.softnetwork.account.model.Account
import app.softnetwork.account.serialization.accountFormats
import app.softnetwork.payment.api.TransactionStatus
import org.json4s.Formats

import scala.language.implicitConversions

/** Created by smanciot on 22/05/2020.
  */
package object serialization {

  val paymentFormats: Formats = accountFormats ++
    Seq(
      GeneratedEnumSerializer(KycDocument.KycDocumentStatus.enumCompanion),
      GeneratedEnumSerializer(KycDocument.KycDocumentType.enumCompanion),
      GeneratedEnumSerializer(UboDeclaration.UboDeclarationStatus.enumCompanion),
      GeneratedEnumSerializer(Transaction.PaymentType.enumCompanion),
      GeneratedEnumSerializer(NaturalUser.NaturalUserType.enumCompanion),
      GeneratedEnumSerializer(LegalUser.LegalUserType.enumCompanion),
      GeneratedEnumSerializer(PaymentAccount.PaymentAccountStatus.enumCompanion),
      GeneratedEnumSerializer(Mandate.MandateStatus.enumCompanion),
      GeneratedEnumSerializer(Mandate.MandateScheme.enumCompanion),
      GeneratedEnumSerializer(Transaction.TransactionNature.enumCompanion),
      GeneratedEnumSerializer(Transaction.TransactionStatus.enumCompanion),
      GeneratedEnumSerializer(Transaction.TransactionType.enumCompanion),
      GeneratedEnumSerializer(RecurringPayment.RecurringPaymentType.enumCompanion),
      GeneratedEnumSerializer(RecurringPayment.RecurringPaymentFrequency.enumCompanion),
      GeneratedEnumSerializer(RecurringPayment.RecurringCardPaymentStatus.enumCompanion),
      GeneratedEnumSerializer(
        SoftPayAccount.Client.Provider.ProviderType.enumCompanion
      )
    )

  implicit def transactionStatusToTransactionResponseStatus(
    transactionStatus: Transaction.TransactionStatus
  ): TransactionStatus = {
    transactionStatus match {
      case Transaction.TransactionStatus.TRANSACTION_CREATED =>
        TransactionStatus.TRANSACTION_CREATED
      case Transaction.TransactionStatus.TRANSACTION_SUCCEEDED =>
        TransactionStatus.TRANSACTION_SUCCEEDED
      case Transaction.TransactionStatus.TRANSACTION_FAILED =>
        TransactionStatus.TRANSACTION_FAILED
      case Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED =>
        TransactionStatus.TRANSACTION_NOT_SPECIFIED
      case Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON =>
        TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
      case Transaction.TransactionStatus.TRANSACTION_CANCELED =>
        TransactionStatus.TRANSACTION_CANCELED
      case Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT =>
        TransactionStatus.TRANSACTION_PENDING_PAYMENT
    }
  }

  implicit def transactionResponseStatusToTransactionStatus(
    transactionResponseStatus: TransactionStatus
  ): Transaction.TransactionStatus = {
    transactionResponseStatus match {
      case TransactionStatus.TRANSACTION_CREATED =>
        Transaction.TransactionStatus.TRANSACTION_CREATED
      case TransactionStatus.TRANSACTION_SUCCEEDED =>
        Transaction.TransactionStatus.TRANSACTION_SUCCEEDED
      case TransactionStatus.TRANSACTION_FAILED =>
        Transaction.TransactionStatus.TRANSACTION_FAILED
      case TransactionStatus.TRANSACTION_NOT_SPECIFIED =>
        Transaction.TransactionStatus.TRANSACTION_NOT_SPECIFIED
      case TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON =>
        Transaction.TransactionStatus.TRANSACTION_FAILED_FOR_TECHNICAL_REASON
      case TransactionStatus.TRANSACTION_CANCELED =>
        Transaction.TransactionStatus.TRANSACTION_CANCELED
      case TransactionStatus.TRANSACTION_PENDING_PAYMENT =>
        Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT
    }
  }

  implicit def accountToSoftPaymentAccount(account: Account): SoftPayAccount = {
    account match {
      case a: SoftPayAccount => a
      case _ =>
        import account._
        SoftPayAccount.defaultInstance
          .withApplications(applications)
          .withCreatedDate(createdDate)
          .withCredentials(credentials)
          .withCurrentProfile(currentProfile.orNull)
          .withDetails(details.orNull)
          .withLastLogin(lastLogin.orNull)
          .withLastUpdated(lastUpdated)
          .withNbLoginFailures(nbLoginFailures)
          .withPrincipal(principal)
          .withProfiles(profiles)
          .withRegistrations(registrations)
          .withSecondaryPrincipals(secondaryPrincipals)
          .withStatus(status)
          .withUuid(uuid)
          .withVerificationCode(verificationCode.orNull)
          .withVerificationToken(verificationToken.orNull)
          .copy(
            anonymous = anonymous,
            fromAnonymous = fromAnonymous
          )
    }
  }
}
