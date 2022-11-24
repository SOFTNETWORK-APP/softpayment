package app.softnetwork.payment

import app.softnetwork.payment.model.RecurringPayment

import scala.language.implicitConversions

package object api {

  implicit def recurringPaymentTypeToRegisterRecurringPaymentType(`type`: RecurringPayment.RecurringPaymentType): RegisterRecurringPaymentRequest.RecurringPaymentType = {
    `type` match {
      case RecurringPayment.RecurringPaymentType.CARD => RegisterRecurringPaymentRequest.RecurringPaymentType.CARD
      case RecurringPayment.RecurringPaymentType.DIRECT_DEBIT => RegisterRecurringPaymentRequest.RecurringPaymentType.DIRECT_DEBIT
      case _ => RegisterRecurringPaymentRequest.RecurringPaymentType.UNKNOWN_PAYMENT_TYPE
    }
  }

  implicit def registerRecurringPaymentTypeToRecurringPaymentType(`type`: RegisterRecurringPaymentRequest.RecurringPaymentType): Option[RecurringPayment.RecurringPaymentType] = {
    `type` match {
      case RegisterRecurringPaymentRequest.RecurringPaymentType.CARD => Some(RecurringPayment.RecurringPaymentType.CARD)
      case RegisterRecurringPaymentRequest.RecurringPaymentType.DIRECT_DEBIT =>
        Some(RecurringPayment.RecurringPaymentType.DIRECT_DEBIT)
      case _ => None
    }
  }

  implicit def recurringPaymentFrequencyToRegisterRecurringPaymentFrequency(frequency: RecurringPayment.RecurringPaymentFrequency): RegisterRecurringPaymentRequest.RecurringPaymentFrequency = {
    frequency match {
      case RecurringPayment.RecurringPaymentFrequency.DAILY =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.DAILY
      case RecurringPayment.RecurringPaymentFrequency.WEEKLY =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.WEEKLY
      case RecurringPayment.RecurringPaymentFrequency.TWICE_A_MONTH =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.TWICE_A_MONTH
      case RecurringPayment.RecurringPaymentFrequency.MONTHLY =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.MONTHLY
      case RecurringPayment.RecurringPaymentFrequency.BIMONTHLY =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.BIMONTHLY
      case RecurringPayment.RecurringPaymentFrequency.QUARTERLY =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.QUARTERLY
      case RecurringPayment.RecurringPaymentFrequency.BIANNUAL =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.BIANNUAL
      case RecurringPayment.RecurringPaymentFrequency.ANNUAL =>
        RegisterRecurringPaymentRequest.RecurringPaymentFrequency.ANNUAL
      case _ => RegisterRecurringPaymentRequest.RecurringPaymentFrequency.UNKNOWN_PAYMENT_FREQUENCY
    }
  }

  implicit def registerRecurringPaymentFrequencyToRecurringPaymentFrequency(frequency: RegisterRecurringPaymentRequest.RecurringPaymentFrequency): Option[RecurringPayment.RecurringPaymentFrequency] = {
    frequency match {
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.DAILY =>
        Some(RecurringPayment.RecurringPaymentFrequency.DAILY)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.WEEKLY =>
        Some(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.TWICE_A_MONTH =>
        Some(RecurringPayment.RecurringPaymentFrequency.TWICE_A_MONTH)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.MONTHLY =>
        Some(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.BIMONTHLY =>
        Some(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.QUARTERLY =>
        Some(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.BIANNUAL =>
        Some(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
      case RegisterRecurringPaymentRequest.RecurringPaymentFrequency.ANNUAL =>
        Some(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
      case _ => None
    }
  }
}
