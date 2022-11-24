package app.softnetwork.payment

import app.softnetwork.payment.model._

import app.softnetwork.protobuf.ScalaPBSerializers
import ScalaPBSerializers.GeneratedEnumSerializer

import org.json4s.Formats

import app.softnetwork.serialization._

import scala.language.implicitConversions

/**
  * Created by smanciot on 22/05/2020.
  */
package object serialization {

  val paymentFormats: Formats = commonFormats ++
      Seq(
          GeneratedEnumSerializer(KycDocument.KycDocumentStatus.enumCompanion),
          GeneratedEnumSerializer(KycDocument.KycDocumentType.enumCompanion),
          GeneratedEnumSerializer(UboDeclaration.UboDeclarationStatus.enumCompanion),
          GeneratedEnumSerializer(Transaction.PaymentType.enumCompanion),
          GeneratedEnumSerializer(PaymentUser.PaymentUserType.enumCompanion),
          GeneratedEnumSerializer(LegalUser.LegalUserType.enumCompanion),
          GeneratedEnumSerializer(PaymentAccount.PaymentAccountStatus.enumCompanion),
          GeneratedEnumSerializer(BankAccount.MandateStatus.enumCompanion),
          GeneratedEnumSerializer(Transaction.TransactionNature.enumCompanion),
          GeneratedEnumSerializer(Transaction.TransactionStatus.enumCompanion),
          GeneratedEnumSerializer(Transaction.TransactionType.enumCompanion),
          GeneratedEnumSerializer(RecurringPayment.RecurringPaymentType.enumCompanion),
          GeneratedEnumSerializer(RecurringPayment.RecurringPaymentFrequency.enumCompanion),
          GeneratedEnumSerializer(RecurringPayment.RecurringCardPaymentStatus.enumCompanion)
      )

}
