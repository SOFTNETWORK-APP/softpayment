package app.softnetwork.payment.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import app.softnetwork.persistence.now
import app.softnetwork.time._

import java.time.LocalDate

class RecurringPaymentSpec extends AnyWordSpec with Matchers {

  "Recurring Payment" must {
    // begin daily frequency
    "compute next daily schedule without end date" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next daily schedule with end date today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next daily schedule with start date yesterday and end date today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withStartDate(now().minusDays(1))
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1)))
        case _ => fail()
      }
    }
    "compute next daily schedule with end date tomorrow" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withEndDate(now().plusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next daily schedule with start date tomorrow and end date tomorrow" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withStartDate(now().plusDays(1))
          .withEndDate(now().plusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusDays(1)))
        case _ => fail()
      }
    }
    "compute next daily schedule with end date in the future and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withEndDate(now().plusDays(1))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusDays(1)))
        case _ => fail()
      }
    }
    "compute next daily schedule with end date today and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "not compute next daily schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next daily schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.DAILY)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    // begin weekly frequency
    "compute next weekly schedule without end date and previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next weekly schedule with end date today and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next weekly schedule with end date in one week and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(now().plusWeeks(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next weekly schedule with end date in one week and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(now().plusWeeks(1))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusWeeks(1)))
        case _ => fail()
      }
    }
    "compute next weekly schedule with end date in one week and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(now().plusWeeks(1))
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1).plusWeeks(1)))
        case _ => fail()
      }
    }
    "compute next weekly schedule with end date the 31th of december this year and previous payment the 31th of december last year" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(LocalDate.of(LocalDate.now().getYear, 12, 31))
          .withLastRecurringPaymentDate(LocalDate.of(LocalDate.now().getYear - 1, 12, 31))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(LocalDate.of(LocalDate.now().getYear, 1, 7)))
        case _ => fail()
      }
    }
    "not compute next weekly schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next weekly schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.WEEKLY)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    // begin monthly frequency
    "compute next monthly schedule without end date and previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next monthly schedule with end date today and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next monthly schedule with end date in one month and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(now().plusMonths(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next monthly schedule with end date in one month and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(now().plusMonths(1))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusMonths(1)))
        case _ => fail()
      }
    }
    "compute next monthly schedule with end date in one month and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(now().plusMonths(1))
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1).plusMonths(1)))
        case _ => fail()
      }
    }
    "compute next monthly schedule with end date the 31th of december this year and previous payment the 31th of december last year" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(LocalDate.of(LocalDate.now().getYear, 12, 31))
          .withLastRecurringPaymentDate(LocalDate.of(LocalDate.now().getYear - 1, 12, 31))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(
            nextPaymentDate.isEqual(LocalDate.of(LocalDate.now().getYear - 1, 12, 31).plusMonths(1))
          )
        case _ => fail()
      }
    }
    "not compute next monthly schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next monthly schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.MONTHLY)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    // begin quarterly frequency
    "compute next quarterly schedule without end date and previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next quarterly schedule with end date today and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next quarterly schedule with end date in 3 months and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(now().plusMonths(3))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next quarterly schedule with end date in 3 months and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(now().plusMonths(3))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusMonths(3)))
        case _ => fail()
      }
    }
    "compute next quarterly schedule with end date in 3 months and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(now().plusMonths(3))
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1).plusMonths(3)))
        case _ => fail()
      }
    }
    "compute next quarterly schedule with end date the 31th of december this year and previous payment the 31th of december last year" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(LocalDate.of(LocalDate.now().getYear, 12, 31))
          .withLastRecurringPaymentDate(LocalDate.of(LocalDate.now().getYear - 1, 12, 31))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(
            nextPaymentDate.isEqual(LocalDate.of(LocalDate.now().getYear - 1, 12, 31).plusMonths(3))
          )
        case _ => fail()
      }
    }
    "not compute next quarterly schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next quarterly schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.QUARTERLY)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    // begin biannual frequency
    "compute next biannual schedule without end date and previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next biannual schedule with end date today and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next biannual schedule with end date in 6 months and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(now().plusMonths(6))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next biannual schedule with end date in 6 months and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(now().plusMonths(6))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusMonths(6)))
        case _ => fail()
      }
    }
    "compute next biannual schedule with end date in 6 months and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(now().plusMonths(6))
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1).plusMonths(6)))
        case _ => fail()
      }
    }
    "compute next biannual schedule with end date the 31th of december this year and previous payment the 31th of december last year" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(LocalDate.of(LocalDate.now().getYear, 12, 31))
          .withLastRecurringPaymentDate(LocalDate.of(LocalDate.now().getYear - 1, 12, 31))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(
            nextPaymentDate.isEqual(LocalDate.of(LocalDate.now().getYear - 1, 12, 31).plusMonths(6))
          )
        case _ => fail()
      }
    }
    "not compute next biannual schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next biannual schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIANNUAL)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    // begin annual frequency
    "compute next annual schedule without end date and previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next annual schedule with end date today and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next annual schedule with end date in 12 months and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(now().plusMonths(12))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next annual schedule with end date in 12 months and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(now().plusMonths(12))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusMonths(12)))
        case _ => fail()
      }
    }
    "compute next annual schedule with end date in 12 months and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(now().plusMonths(12))
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1).plusMonths(12)))
        case _ => fail()
      }
    }
    "compute next annual schedule with end date the 31th of december this year and previous payment the 31th of december last year" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(LocalDate.of(LocalDate.now().getYear, 12, 31))
          .withLastRecurringPaymentDate(LocalDate.of(LocalDate.now().getYear - 1, 12, 31))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(
            nextPaymentDate.isEqual(
              LocalDate.of(LocalDate.now().getYear - 1, 12, 31).plusMonths(12)
            )
          )
        case _ => fail()
      }
    }
    "not compute next annual schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next annual schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.ANNUAL)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    // begin bimonthly frequency
    "compute next bimonthly schedule without end date and previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next bimonthly schedule with end date today and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next bimonthly schedule with end date in 2 months and no previous payment" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(now().plusMonths(2))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now()))
        case _ => fail()
      }
    }
    "compute next bimonthly schedule with end date in 2 months and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(now().plusMonths(2))
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().plusMonths(2)))
        case _ => fail()
      }
    }
    "compute next bimonthly schedule with end date in 2 months and previous payment yesterday" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(now().plusMonths(2))
          .withLastRecurringPaymentDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(nextPaymentDate.isEqual(now().minusDays(1).plusMonths(2)))
        case _ => fail()
      }
    }
    "compute next bimonthly schedule with end date the 31th of december this year and previous payment the 31th of december last year" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(LocalDate.of(LocalDate.now().getYear, 12, 31))
          .withLastRecurringPaymentDate(LocalDate.of(LocalDate.now().getYear - 1, 12, 31))
      recurringPayment.nextPaymentDate match {
        case Some(nextPaymentDate) =>
          assert(
            nextPaymentDate.isEqual(LocalDate.of(LocalDate.now().getYear - 1, 12, 31).plusMonths(2))
          )
        case _ => fail()
      }
    }
    "not compute next bimonthly schedule with end date today and previous payment today" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(now())
          .withLastRecurringPaymentDate(now())
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
    "not compute next bimonthly schedule with end date in the past" in {
      val recurringPayment =
        RecurringPayment.defaultInstance
          .withFrequency(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY)
          .withEndDate(now().minusDays(1))
      recurringPayment.nextPaymentDate match {
        case Some(_) => fail()
        case _       =>
      }
    }
  }
}
