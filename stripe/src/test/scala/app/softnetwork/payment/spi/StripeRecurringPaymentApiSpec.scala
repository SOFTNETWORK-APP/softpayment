package app.softnetwork.payment.spi

import app.softnetwork.payment.config.{StripeApi, StripeSettings}
import app.softnetwork.payment.model.{RecurringPayment, SoftPayAccount}
import app.softnetwork.payment.serialization.paymentFormats
import com.typesafe.scalalogging.Logger
import org.json4s.Formats
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory

class StripeRecurringPaymentApiSpec extends AnyWordSpec with Matchers {

  // Minimal concrete instance to test private[spi] helpers
  private object TestApi extends StripeRecurringPaymentApi with StripeContext {
    override protected def mlog: Logger = Logger(LoggerFactory.getLogger(getClass))
    override implicit def provider: SoftPayAccount.Client.Provider =
      StripeSettings.StripeApiConfig.softPayProvider
    override implicit def config: StripeApi.Config = StripeSettings.StripeApiConfig
    override implicit def formats: Formats = paymentFormats
  }

  "toStripeInterval" should {
    "map DAILY to day/1" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.DAILY) shouldBe ("day", 1L)
    }
    "map WEEKLY to week/1" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.WEEKLY) shouldBe ("week", 1L)
    }
    "map TWICE_A_MONTH to week/2 (biweekly approximation)" in {
      TestApi.toStripeInterval(
        RecurringPayment.RecurringPaymentFrequency.TWICE_A_MONTH
      ) shouldBe ("week", 2L)
    }
    "map MONTHLY to month/1" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.MONTHLY) shouldBe ("month", 1L)
    }
    "map BIMONTHLY to month/2" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.BIMONTHLY) shouldBe ("month", 2L)
    }
    "map QUARTERLY to month/3" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.QUARTERLY) shouldBe ("month", 3L)
    }
    "map BIANNUAL to month/6" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.BIANNUAL) shouldBe ("month", 6L)
    }
    "map ANNUAL to year/1" in {
      TestApi.toStripeInterval(RecurringPayment.RecurringPaymentFrequency.ANNUAL) shouldBe ("year", 1L)
    }
  }

  "toRecurringCardPaymentStatus" should {
    "map active to IN_PROGRESS" in {
      TestApi.toRecurringCardPaymentStatus("active") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.IN_PROGRESS
    }
    "map trialing to IN_PROGRESS" in {
      TestApi.toRecurringCardPaymentStatus("trialing") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.IN_PROGRESS
    }
    "map incomplete with requiresAction to AUTHENTICATION_NEEDED" in {
      TestApi.toRecurringCardPaymentStatus("incomplete", requiresAction = true) shouldBe
        RecurringPayment.RecurringCardPaymentStatus.AUTHENTICATION_NEEDED
    }
    "map incomplete without requiresAction to CREATED" in {
      TestApi.toRecurringCardPaymentStatus("incomplete") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.CREATED
    }
    "map past_due to CREATED" in {
      TestApi.toRecurringCardPaymentStatus("past_due") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.CREATED
    }
    "map unpaid to CREATED" in {
      TestApi.toRecurringCardPaymentStatus("unpaid") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.CREATED
    }
    "map canceled to ENDED" in {
      TestApi.toRecurringCardPaymentStatus("canceled") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.ENDED
    }
    "map incomplete_expired to ENDED" in {
      TestApi.toRecurringCardPaymentStatus("incomplete_expired") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.ENDED
    }
    "map paused to ENDED" in {
      TestApi.toRecurringCardPaymentStatus("paused") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.ENDED
    }
    "map unknown status to CREATED" in {
      TestApi.toRecurringCardPaymentStatus("some_unknown_status") shouldBe
        RecurringPayment.RecurringCardPaymentStatus.CREATED
    }
  }
}
