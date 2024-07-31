package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{
  BankAccount,
  DirectDebitTransaction,
  MandateResult,
  Transaction
}
import app.softnetwork.persistence
import app.softnetwork.time.dateToInstant
import com.stripe.model.SetupIntent
import com.stripe.param.SetupIntentCreateParams

import java.util.Date
import scala.util.{Failure, Success, Try}

trait StripeDirectDebitApi extends DirectDebitApi { _: StripeContext =>

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
    idempotencyKey: Option[String]
  ): Option[MandateResult] = {
    Try {
      val params =
        SetupIntentCreateParams
          .builder()
          // sepa direct debit
          .setPaymentMethodOptions(
            SetupIntentCreateParams.PaymentMethodOptions
              .builder()
              .setSepaDebit(
                SetupIntentCreateParams.PaymentMethodOptions.SepaDebit
                  .builder()
                  .setMandateOptions(
                    SetupIntentCreateParams.PaymentMethodOptions.SepaDebit.MandateOptions
                      .builder()
                      .build()
                  )
                  .build()
              )
              .build()
          )
          // payment method
          .setPaymentMethod(
            bankAccountId
          )
          /*.setAutomaticPaymentMethods(
            SetupIntentCreateParams.AutomaticPaymentMethods.builder().setEnabled(true).build()
          )*/
          // automatic confirmation
          .setConfirm(true)
          .setMandateData(
            SetupIntentCreateParams.MandateData
              .builder()
              .setCustomerAcceptance(
                SetupIntentCreateParams.MandateData.CustomerAcceptance
                  .builder()
                  .setAcceptedAt(persistence.now().getEpochSecond)
                  .setOffline(
                    SetupIntentCreateParams.MandateData.CustomerAcceptance.Offline
                      .builder()
                      .build()
                  )
                  .build()
              )
              .build()
          )
          // on behalf of
          .setOnBehalfOf(userId)
          // off session usage
          .setUsage(SetupIntentCreateParams.Usage.OFF_SESSION)
          // the mandate will be attached to the in-context stripe account
          .setAttachToSelf(true)
          // return url
          .setReturnUrl(
            s"${config.mandateReturnUrl}?externalUuid=$externalUuid&idempotencyKey=${idempotencyKey
              .getOrElse("")}"
          )
          .build()

      SetupIntent.create(params, StripeApi().requestOptions)

    } match {
      case Success(setupIntent) =>
        val status = setupIntent.getStatus
        var mandate =
          MandateResult.defaultInstance
            .withId(setupIntent.getId)
            .withResultCode(status)
        status match {
          case "succeeded" =>
            mandate = mandate.withStatus(BankAccount.MandateStatus.MANDATE_ACTIVATED)
          case "requires_action" =>
            mandate = mandate
              .withStatus(BankAccount.MandateStatus.MANDATE_CREATED)
              .withRedirectUrl(setupIntent.getNextAction.getRedirectToUrl.getUrl)
              .withResultMessage(setupIntent.getNextAction.getType)
          case "processing" =>
            mandate = mandate
              .withStatus(BankAccount.MandateStatus.MANDATE_SUBMITTED)
          case "canceled" =>
            mandate = mandate
              .withStatus(BankAccount.MandateStatus.MANDATE_EXPIRED)
              .withResultMessage("the confirmation limit has been reached")
          case _ =>
            mandate = mandate
              .withStatus(BankAccount.MandateStatus.MANDATE_FAILED)
              .withResultMessage("the mandate has failed")
        }

        Some(mandate)

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
    *   mandate associated to this bank account
    */
  override def loadMandate(
    maybeMandateId: Option[String],
    userId: String,
    bankAccountId: String
  ): Option[MandateResult] = {
    maybeMandateId match {
      case Some(mandateId) =>
        Try(SetupIntent.retrieve(mandateId, StripeApi().requestOptions)) match {
          case Success(setupIntent) =>
            Some(
              MandateResult.defaultInstance
                .withId(setupIntent.getId)
                .withStatus(
                  setupIntent.getStatus match {
                    case "succeeded"  => BankAccount.MandateStatus.MANDATE_ACTIVATED
                    case "processing" => BankAccount.MandateStatus.MANDATE_SUBMITTED
                    case "canceled"   => BankAccount.MandateStatus.MANDATE_EXPIRED
                    case _            => BankAccount.MandateStatus.MANDATE_FAILED
                  }
                )
            )
          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
  }

  /** @param mandateId
    *   - Provider mandate id
    * @return
    *   mandate result
    */
  override def cancelMandate(mandateId: String): Option[MandateResult] = {
    Try {
      val requestOptions = StripeApi().requestOptions
      SetupIntent.retrieve(mandateId, requestOptions).cancel(requestOptions)
    } match {
      case Success(setupIntent) =>
        Some(
          MandateResult.defaultInstance
            .withId(setupIntent.getId)
            .withStatus(BankAccount.MandateStatus.MANDATE_EXPIRED)
        )
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
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
    idempotency: Option[Boolean]
  ): Option[Transaction] = None //TODO

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
  ): Option[Transaction] = None //TODO
}
