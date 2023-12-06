package app.softnetwork.payment.model

import app.softnetwork.time._
import app.softnetwork.persistence.now

import java.time.LocalDate
import java.util.Date

import scala.language.implicitConversions

trait RecurringPaymentDecorator { _: RecurringPayment =>
  lazy val nextPaymentDate: Option[LocalDate] = {
    if (`type`.isCard && getCardStatus.isEnded) {
      None
    } else {
      val d: Date = startDate.getOrElse(now())
      val today: LocalDate = d
      val maybePreviousPayment: Option[LocalDate] = lastRecurringPaymentDate match {
        case Some(value) => Some(value)
        case _           => None
      }

      frequency match {
        case Some(f) =>
          f match {
            // begin daily frequency
            case RecurringPayment.RecurringPaymentFrequency.DAILY =>
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) => Some(previousPayment.plusDays(1))
                      case _                     => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        Some(today).filter(previousPayment.isBefore)
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusDays(1))
                    case _                     => Some(today)
                  }
              }
            // begin weekly frequency
            case RecurringPayment.RecurringPaymentFrequency.WEEKLY =>
              val currentWeekNumber: Int = weekNumber(today)
              val currentYear: Int = today.getYear
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (weekNumber(
                            previousPayment
                          ) < currentWeekNumber || previousYear < currentYear)
                        ) {
                          val nextWeek = previousPayment.plusWeeks(1)
                          if (nextWeek.isBefore(end) || nextWeek.isEqual(end)) {
                            Some(nextWeek)
                          } else {
                            Some(today)
                          }
                        } else {
                          val nextWeek = previousPayment.plusWeeks(1)
                          if (nextWeek.isBefore(end) || nextWeek.isEqual(end)) {
                            Some(nextWeek)
                          } else {
                            None
                          }
                        }
                      case _ => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (weekNumber(
                            previousPayment
                          ) < currentWeekNumber || previousYear < currentYear)
                        ) {
                          Some(today)
                        } else
                          None
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusWeeks(1))
                    case _                     => Some(today)
                  }
              }
            // begin monthly frequency
            case RecurringPayment.RecurringPaymentFrequency.MONTHLY =>
              val currentMonthValue: Int = today.getMonthValue
              val currentYear: Int = today.getYear
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(
                            today
                          ) && (previousPayment.getMonthValue < currentMonthValue || previousYear < currentYear)
                        ) {
                          val nextMonth = previousPayment.plusMonths(1)
                          if (nextMonth.isBefore(end) || nextMonth.isEqual(end)) {
                            Some(nextMonth)
                          } else {
                            Some(today)
                          }
                        } else {
                          val nextMonth = previousPayment.plusMonths(1)
                          if (nextMonth.isBefore(end) || nextMonth.isEqual(end)) {
                            Some(nextMonth)
                          } else {
                            None
                          }
                        }
                      case _ => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(
                            today
                          ) && (previousPayment.getMonthValue < currentMonthValue || previousYear < currentYear)
                        ) {
                          val nextMonth = previousPayment.plusMonths(1)
                          if (nextMonth.isBefore(end) || nextMonth.isEqual(end)) {
                            Some(nextMonth)
                          } else {
                            Some(today)
                          }
                        } else
                          None
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusMonths(1))
                    case _                     => Some(today)
                  }
              }
            // begin bimonthly frequency
            case RecurringPayment.RecurringPaymentFrequency.BIMONTHLY =>
              val currentBimonthly: Int = bimonthlyNumber(today)
              val currentYear: Int = today.getYear
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (bimonthlyNumber(
                            previousPayment
                          ) < currentBimonthly || previousYear < currentYear)
                        ) {
                          val nextBimonthly = previousPayment.plusMonths(2)
                          if (nextBimonthly.isBefore(end) || nextBimonthly.isEqual(end)) {
                            Some(nextBimonthly)
                          } else {
                            Some(today)
                          }
                        } else {
                          val nextBimonthly = previousPayment.plusMonths(2)
                          if (nextBimonthly.isBefore(end) || nextBimonthly.isEqual(end)) {
                            Some(nextBimonthly)
                          } else {
                            None
                          }
                        }
                      case _ => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (bimonthlyNumber(
                            previousPayment
                          ) < currentBimonthly || previousYear < currentYear)
                        ) {
                          val nextBimonthly = previousPayment.plusMonths(2)
                          if (nextBimonthly.isBefore(end) || nextBimonthly.isEqual(end)) {
                            Some(nextBimonthly)
                          } else {
                            Some(today)
                          }
                        } else
                          None
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusMonths(2))
                    case _                     => Some(today)
                  }
              }
            // begin quarterly frequency
            case RecurringPayment.RecurringPaymentFrequency.QUARTERLY =>
              val currentQuarter: Int = quarterNumber(today)
              val currentYear: Int = today.getYear
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (quarterNumber(
                            previousPayment
                          ) < currentQuarter || previousYear < currentYear)
                        ) {
                          val nextQuarter = previousPayment.plusMonths(3)
                          if (nextQuarter.isBefore(end) || nextQuarter.isEqual(end)) {
                            Some(nextQuarter)
                          } else {
                            Some(today)
                          }
                        } else {
                          val nextQuarter = previousPayment.plusMonths(3)
                          if (nextQuarter.isBefore(end) || nextQuarter.isEqual(end)) {
                            Some(nextQuarter)
                          } else {
                            None
                          }
                        }
                      case _ => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (quarterNumber(
                            previousPayment
                          ) < currentQuarter || previousYear < currentYear)
                        ) {
                          val nextQuarter = previousPayment.plusMonths(3)
                          if (nextQuarter.isBefore(end) || nextQuarter.isEqual(end)) {
                            Some(nextQuarter)
                          } else {
                            Some(today)
                          }
                        } else
                          None
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusMonths(3))
                    case _                     => Some(today)
                  }
              }
            // begin biannual frequency
            case RecurringPayment.RecurringPaymentFrequency.BIANNUAL =>
              val currentSemester: Int = semesterNumber(today)
              val currentYear: Int = today.getYear
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (semesterNumber(
                            previousPayment
                          ) < currentSemester || previousYear < currentYear)
                        ) {
                          val nextQuarter = previousPayment.plusMonths(6)
                          if (nextQuarter.isBefore(end) || nextQuarter.isEqual(end)) {
                            Some(nextQuarter)
                          } else {
                            Some(today)
                          }
                        } else {
                          val nextQuarter = previousPayment.plusMonths(6)
                          if (nextQuarter.isBefore(end) || nextQuarter.isEqual(end)) {
                            Some(nextQuarter)
                          } else {
                            None
                          }
                        }
                      case _ => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (
                          previousPayment.isBefore(today) && (semesterNumber(
                            previousPayment
                          ) < currentSemester || previousYear < currentYear)
                        ) {
                          val nextQuarter = previousPayment.plusMonths(6)
                          if (nextQuarter.isBefore(end) || nextQuarter.isEqual(end)) {
                            Some(nextQuarter)
                          } else {
                            Some(today)
                          }
                        } else
                          None
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusMonths(6))
                    case _                     => Some(today)
                  }
              }
            // begin annual frequency
            case RecurringPayment.RecurringPaymentFrequency.ANNUAL =>
              val currentYear: Int = today.getYear
              endDate match {
                case Some(end) =>
                  if (end.isAfter(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (previousPayment.isBefore(today) && previousYear < currentYear) {
                          val nextYear = previousPayment.plusMonths(12)
                          if (nextYear.isBefore(end) || nextYear.isEqual(end)) {
                            Some(nextYear)
                          } else {
                            Some(today)
                          }
                        } else {
                          val nextYear = previousPayment.plusMonths(12)
                          if (nextYear.isBefore(end) || nextYear.isEqual(end)) {
                            Some(nextYear)
                          } else {
                            None
                          }
                        }
                      case _ => Some(today)
                    }
                  } else if (end.isEqual(today)) {
                    maybePreviousPayment match {
                      case Some(previousPayment) =>
                        val previousYear: Int = previousPayment.getYear
                        if (previousPayment.isBefore(today) && previousYear < currentYear) {
                          val nextYear = previousPayment.plusMonths(12)
                          if (nextYear.isBefore(end) || nextYear.isEqual(end)) {
                            Some(nextYear)
                          } else {
                            Some(today)
                          }
                        } else
                          None
                      case _ => Some(today)
                    }
                  } else {
                    None
                  }
                case _ =>
                  maybePreviousPayment match {
                    case Some(previousPayment) => Some(previousPayment.plusMonths(12))
                    case _                     => Some(today)
                  }
              }
            case _ => None //TODO annual, ...
          }
        case _ => None
      }
    }
  }

  lazy val view: RecurringPaymentView = RecurringPaymentView(this)
}

case class RecurringPaymentView(
  id: Option[String] = None,
  createdDate: java.util.Date,
  lastUpdated: java.util.Date,
  firstDebitedAmount: Int,
  firstFeesAmount: Int,
  currency: String,
  `type`: RecurringPayment.RecurringPaymentType,
  cardStatus: Option[RecurringPayment.RecurringCardPaymentStatus] = None,
  startDate: Option[java.util.Date] = None,
  endDate: Option[java.util.Date] = None,
  frequency: Option[RecurringPayment.RecurringPaymentFrequency] = None,
  nextRecurringPaymentDate: Option[java.util.Date] = None,
  fixedNextAmount: Option[Boolean] = None,
  nextDebitedAmount: Option[Int] = None,
  nextFeesAmount: Option[Int] = None,
  lastRecurringPaymentTransactionId: Option[String] = None,
  lastRecurringPaymentDate: Option[java.util.Date] = None,
  numberOfRecurringPayments: Option[Int] = None,
  cumulatedDebitedAmount: Option[Int] = None,
  cumulatedFeesAmount: Option[Int] = None
)

object RecurringPaymentView {
  def apply(recurringPayment: RecurringPayment): RecurringPaymentView = {
    import recurringPayment._
    RecurringPaymentView(
      id,
      createdDate,
      lastUpdated,
      firstDebitedAmount,
      firstFeesAmount,
      currency,
      `type`,
      cardStatus,
      startDate,
      endDate,
      frequency,
      nextRecurringPaymentDate,
      fixedNextAmount,
      nextDebitedAmount,
      nextFeesAmount,
      lastRecurringPaymentTransactionId,
      lastRecurringPaymentDate,
      numberOfRecurringPayments,
      cumulatedDebitedAmount,
      cumulatedFeesAmount
    )
  }
}
