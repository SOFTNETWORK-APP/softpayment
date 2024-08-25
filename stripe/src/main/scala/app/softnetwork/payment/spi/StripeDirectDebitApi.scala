package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{DirectDebitTransaction, Mandate, MandateResult, Transaction}
import app.softnetwork.persistence
import app.softnetwork.serialization.asJson
import app.softnetwork.time.dateToInstant
import com.google.gson.Gson
import com.stripe.model.{
  Account,
  Customer,
  Mandate => StripeMandate,
  PaymentIntent,
  PaymentMethod,
  SetupIntent
}
import com.stripe.param.{
  AccountUpdateParams,
  CustomerCreateParams,
  PaymentIntentCreateParams,
  PaymentMethodAttachParams,
  PaymentMethodCreateParams,
  PaymentMethodListParams,
  SetupIntentCreateParams
}

import java.net.URLEncoder
import java.util.Date
import scala.util.{Failure, Success, Try}
import collection.JavaConverters._
import scala.language.implicitConversions

trait StripeDirectDebitApi extends DirectDebitApi {
  _: StripeContext =>

  /** @param externalUuid
    *   - external unique id
    * @param userId
    *   - Provider user id
    * @param bankAccountId
    *   - optional Bank account id to associate with this mandate (required by some providers
    *     including MangoPay)
    * @param iban
    *   - optional IBAN to associate with this mandate (required by some providers including Stripe)
    * @param idempotencyKey
    *   - whether to use an idempotency key for this request or not
    * @return
    *   mandate result
    */
  override def mandate(
    externalUuid: String,
    userId: String,
    bankAccountId: Option[String],
    iban: Option[String],
    idempotencyKey: Option[String]
  ): Option[MandateResult] = {
    Try {
      val requestOptions = StripeApi().requestOptionsBuilder /*.setStripeAccount(userId)*/.build()

      val params =
        SetupIntentCreateParams
          .builder()
          .addPaymentMethodType("sepa_debit")
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
          // off session usage
          .setUsage(SetupIntentCreateParams.Usage.OFF_SESSION)

      val acct = """acct_.*""".r

      val cus = """cus_.*""".r

      ((userId match {
        case acct() =>
          val account = Account.retrieve(userId, requestOptions)
          account.getMetadata.asScala.get("customer_id") match {
            case Some(customerId) =>
              Some(Customer.retrieve(customerId, requestOptions))
            case None =>
              val params =
                CustomerCreateParams
                  .builder()
                  .putMetadata("account_id", account.getId)
                  .putMetadata("external_uuid", externalUuid)
              account.getBusinessType match {
                case "individual" =>
                  params
                    .setName(
                      s"${account.getIndividual.getFirstName} ${account.getIndividual.getLastName}"
                    )
                  Option(account.getEmail) match {
                    case Some(email) =>
                      params
                        .setEmail(email)
                    case None =>
                  }
                  Option(account.getIndividual.getAddress) match {
                    case Some(address) =>
                      params
                        .setAddress(
                          CustomerCreateParams.Address
                            .builder()
                            .setCity(address.getCity)
                            .setCountry(address.getCountry)
                            .setLine1(address.getLine1)
                            .setLine2(address.getLine2)
                            .setPostalCode(address.getPostalCode)
                            .setState(address.getState)
                            .build()
                        )
                    case None =>
                  }
                  Option(account.getIndividual.getPhone) match {
                    case Some(phone) =>
                      params
                        .setPhone(phone)
                    case None =>
                  }

                case "company" =>
                  params.setName(account.getCompany.getName)
                  Option(account.getEmail) match {
                    case Some(email) =>
                      params
                        .setEmail(email)
                    case None =>
                  }
                  Option(account.getCompany.getAddress) match {
                    case Some(address) =>
                      params
                        .setAddress(
                          CustomerCreateParams.Address
                            .builder()
                            .setCity(address.getCity)
                            .setCountry(address.getCountry)
                            .setLine1(address.getLine1)
                            .setLine2(address.getLine2)
                            .setPostalCode(address.getPostalCode)
                            .setState(address.getState)
                            .build()
                        )
                    case None =>
                  }
                  Option(account.getCompany.getPhone) match {
                    case Some(phone) =>
                      params
                        .setPhone(phone)
                    case None =>
                  }
              }
              mlog.info(s"Creating customer -> ${new Gson().toJson(params.build())}")
              val customer = Customer.create(params.build(), requestOptions)
              mlog.info(s"Customer created -> ${customer.getId}")
              account.update(
                AccountUpdateParams
                  .builder()
                  .putMetadata("customer_id", customer.getId)
                  .build(),
                requestOptions
              )
              mlog.info(s"Account updated -> ${account.getId}")
              Some(customer)
          }
        case cus() =>
          Some(Customer.retrieve(userId, requestOptions))
        case _ =>
          None
      }) match {
        case Some(customer) =>
          params.setCustomer(customer.getId)
          val paymentMethods = PaymentMethod.list(
            PaymentMethodListParams
              .builder()
              .setCustomer(customer.getId)
              .setType(PaymentMethodListParams.Type.SEPA_DEBIT)
              .build(),
            requestOptions
          )
          paymentMethods.getData.asScala.headOption match {
            case Some(paymentMethod) =>
              Some(paymentMethod)
            case None =>
              iban match {
                case Some(iban) =>
                  val billing =
                    PaymentMethodCreateParams.BillingDetails.builder()
                  billing.setName(customer.getName)
                  Option(customer.getEmail) match {
                    case Some(email) =>
                      billing
                        .setEmail(email)
                    case None =>
                  }
                  Option(customer.getAddress) match {
                    case Some(address) =>
                      billing
                        .setAddress(
                          PaymentMethodCreateParams.BillingDetails.Address
                            .builder()
                            .setCity(address.getCity)
                            .setCountry(address.getCountry)
                            .setLine1(address.getLine1)
                            .setLine2(address.getLine2)
                            .setPostalCode(address.getPostalCode)
                            .setState(address.getState)
                            .build()
                        )
                    case None =>
                  }
                  val params =
                    PaymentMethodCreateParams
                      .builder()
                      .setType(PaymentMethodCreateParams.Type.SEPA_DEBIT)
                      .setBillingDetails(billing.build())
                      .setSepaDebit(
                        PaymentMethodCreateParams.SepaDebit.builder().setIban(iban).build()
                      )
                  mlog.info(s"Creating payment method -> ${new Gson().toJson(params.build())}")
                  var payment = PaymentMethod.create(params.build(), requestOptions)
                  mlog.info(s"Payment method created -> ${new Gson().toJson(payment)}")
                  payment = payment.attach(
                    PaymentMethodAttachParams.builder().setCustomer(customer.getId).build(),
                    requestOptions
                  )
                  mlog.info(s"Payment method attached to customer -> ${new Gson().toJson(payment)}")
                  Some(payment)
                case None =>
                  None
              }
          }
        case None =>
          None
      }) match {
        case Some(paymentMethod) =>
          params
            .setMandateData(
              SetupIntentCreateParams.MandateData
                .builder()
                .setCustomerAcceptance(
                  SetupIntentCreateParams.MandateData.CustomerAcceptance
                    .builder()
                    .setAcceptedAt(persistence.now().getEpochSecond)
                    .setType(SetupIntentCreateParams.MandateData.CustomerAcceptance.Type.OFFLINE)
                    .setOffline(
                      SetupIntentCreateParams.MandateData.CustomerAcceptance.Offline
                        .builder()
                        .build()
                    )
                    .build()
                )
                .build()
            )
            // payment method
            .setPaymentMethod(
              paymentMethod.getId
            )
            /*.setAutomaticPaymentMethods(
              SetupIntentCreateParams.AutomaticPaymentMethods.builder().setEnabled(true).build()
            )*/
            // automatic confirmation
            .setConfirm(true)
            // return url
            .setReturnUrl(
              s"${config.mandateReturnUrl}?externalUuid=${URLEncoder
                .encode(externalUuid, "UTF-8")}&idempotencyKey=${idempotencyKey
                .getOrElse("")}"
            )
        case None =>
      }

      SetupIntent.create(
        params.build(),
        requestOptions
      )

    } match {
      case Success(setupIntent) =>
        mlog.info(s"Creating mandate -> ${new Gson().toJson(setupIntent)}")
        val mandate: MandateResult = setupIntent
        mlog.info(s"Mandate created -> ${asJson(mandate)}")
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
    bankAccountId: Option[String]
  ): Option[MandateResult] = {
    maybeMandateId match {
      case Some(mandateId) =>
        Try {
          val requestOptions = StripeApi().requestOptions
          val seti = """seti_.*""".r
          mandateId match {
            case seti() =>
              val setupIntent = SetupIntent.retrieve(mandateId, requestOptions)
              Option(setupIntent.getMandate) match {
                case Some(mandateId) =>
                  StripeMandate.retrieve(mandateId, requestOptions)
                case None =>
                  setupIntent
              }
            case _ =>
              StripeMandate.retrieve(mandateId, requestOptions)
          }
        } match {
          case Success(mandate: StripeMandate) =>
            Some(
              MandateResult.defaultInstance
                .withId(mandate.getId)
                .withStatus(
                  mandate.getStatus match {
                    case "active"   => Mandate.MandateStatus.MANDATE_ACTIVATED
                    case "pending"  => Mandate.MandateStatus.MANDATE_SUBMITTED
                    case "inactive" => Mandate.MandateStatus.MANDATE_EXPIRED
                    case _          => Mandate.MandateStatus.MANDATE_FAILED
                  }
                )
            )
          case Success(setupIntent: SetupIntent) =>
            Some(setupIntent)
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
      Option(StripeMandate.retrieve(mandateId, requestOptions)) match {
        case Some(mandate) =>
          PaymentMethod.retrieve(mandate.getPaymentMethod, requestOptions).detach(requestOptions)
        case _ => None
      }
    } match {
      case Success(_) =>
        Some(
          MandateResult.defaultInstance
            .withId(mandateId)
            .withStatus(Mandate.MandateStatus.MANDATE_EXPIRED)
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
  ): Option[Transaction] = {
    maybeDirectDebitTransaction match {
      case Some(directDebitTransaction) =>
        mlog.info(
          s"Creating direct debit transaction -> ${asJson(directDebitTransaction)}"
        )
        Try {
          val requestOptions = StripeApi().requestOptions
          Option(StripeMandate.retrieve(directDebitTransaction.mandateId, requestOptions)) match {
            case Some(mandate) =>
              val payment = PaymentMethod.retrieve(mandate.getPaymentMethod, requestOptions)

              val params = PaymentIntentCreateParams
                .builder()
                .addPaymentMethodType("sepa_debit")
                .setPaymentMethod(mandate.getPaymentMethod)
                .setAmount(directDebitTransaction.debitedAmount)
                .setApplicationFeeAmount(directDebitTransaction.feesAmount)
                .setCaptureMethod(PaymentIntentCreateParams.CaptureMethod.AUTOMATIC)
                .setCurrency(directDebitTransaction.currency)
                .setCustomer(payment.getCustomer)
                .setConfirm(true)
                .setMandate(mandate.getId)
                .setOffSession(true)
                .setTransferData(
                  PaymentIntentCreateParams.TransferData
                    .builder()
                    .setDestination(directDebitTransaction.creditedUserId)
                    .build()
                )
                .putMetadata("transaction_type", "direct_debit")
                .putMetadata("payment_type", "direct_debited")
                .putMetadata("mandate_id", mandate.getId)

              if (directDebitTransaction.statementDescriptor.nonEmpty) {
                params.setStatementDescriptor(directDebitTransaction.statementDescriptor)
              }

              PaymentIntent.create(params.build(), requestOptions)
            case _ => throw new Exception("Mandate not found")
          }
        } match {
          case Success(payment) =>
            val status = payment.getStatus
            var transaction =
              Transaction()
                .withId(payment.getId)
                .withNature(Transaction.TransactionNature.REGULAR)
                .withType(Transaction.TransactionType.DIRECT_DEBIT)
                .withPaymentType(Transaction.PaymentType.DIRECT_DEBITED)
                .withAmount(payment.getAmount.intValue())
                .withFees(Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0))
                .withCurrency(payment.getCurrency)
                .withMandateId(directDebitTransaction.mandateId)
                .withResultCode(status)
                .withAuthorId(payment.getCustomer)
                .withCreditedUserId(
                  payment.getTransferData.getDestination
                )
                .withCreditedWalletId(directDebitTransaction.creditedWalletId)

            status match {
              case "requires_action" if payment.getNextAction.getType == "redirect_to_url" =>
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_CREATED,
                  //The URL you must redirect your customer to in order to authenticate the payment.
                  redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl)
                )
              case "requires_payment_method" =>
                transaction = transaction.copy(
                  status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
                  paymentClientSecret = Option(payment.getClientSecret),
                  paymentClientReturnUrl = None
                )
              case "succeeded" | "requires_capture" =>
                transaction =
                  transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
              case _ =>
                transaction =
                  transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
            }

            mlog.info(
              s"Direct debit created -> ${asJson(transaction)}"
            )

            Some(transaction)

          case Failure(f) =>
            mlog.error(f.getMessage, f)
            None
        }
      case _ => None
    }
  }

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
  ): Option[Transaction] = {
    Try {
      PaymentIntent.retrieve(transactionId, StripeApi().requestOptions)
    } match {
      case Success(payment) =>
        val metadata = payment.getMetadata.asScala
        val status = payment.getStatus
        var transaction =
          Transaction()
            .withId(payment.getId)
            .withNature(Transaction.TransactionNature.REGULAR)
            .withType(Transaction.TransactionType.DIRECT_DEBIT)
            .withPaymentType(Transaction.PaymentType.DIRECT_DEBITED)
            .withAmount(payment.getAmount.intValue())
            .withFees(Option(payment.getApplicationFeeAmount).map(_.intValue()).getOrElse(0))
            .withCurrency(payment.getCurrency)
            .withResultCode(status)
            .withAuthorId(payment.getCustomer)
            .withCreditedUserId(
              payment.getTransferData.getDestination
            )
            .withCreditedWalletId(walletId)
            .copy(
              mandateId = metadata.get("mandate_id")
            )

        status match {
          case "requires_action" if payment.getNextAction.getType == "redirect_to_url" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_CREATED,
              //The URL you must redirect your customer to in order to authenticate the payment.
              redirectUrl = Option(payment.getNextAction.getRedirectToUrl.getUrl)
            )
          case "requires_payment_method" =>
            transaction = transaction.copy(
              status = Transaction.TransactionStatus.TRANSACTION_PENDING_PAYMENT,
              paymentClientSecret = Option(payment.getClientSecret),
              paymentClientReturnUrl = None
            )
          case "succeeded" | "requires_capture" =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_SUCCEEDED)
          case _ =>
            transaction =
              transaction.copy(status = Transaction.TransactionStatus.TRANSACTION_CREATED)
        }

        mlog.info(
          s"Direct debit transaction retrieved -> ${asJson(transaction)}"
        )

        Some(transaction)
      case Failure(f) =>
        mlog.error(f.getMessage, f)
        None
    }
  }

  implicit def setupIntentToMandateResult(setupIntent: SetupIntent): MandateResult = {
    val status = setupIntent.getStatus
    var mandate =
      MandateResult.defaultInstance
        .withId(Option(setupIntent.getMandate).getOrElse(setupIntent.getId))
        .withResultCode(status)
    status match {
      case "succeeded" =>
        mandate = mandate.withStatus(Mandate.MandateStatus.MANDATE_ACTIVATED)
      case "requires_action" if setupIntent.getNextAction.getType == "redirect_to_url" =>
        mandate = mandate
          .withStatus(Mandate.MandateStatus.MANDATE_CREATED)
          .withRedirectUrl(setupIntent.getNextAction.getRedirectToUrl.getUrl)
          .withResultMessage(setupIntent.getNextAction.getType)
      case "requires_payment_method" | "requires_confirmation" =>
        mandate = mandate
          .withStatus(Mandate.MandateStatus.MANDATE_PENDING)
          .copy(
            mandateClientSecret = Option(setupIntent.getClientSecret),
            mandateClientReturnUrl = Option(
              s"${config.mandateReturnUrl}?MandateId=${mandate.id}"
            )
          )
      case "processing" =>
        mandate = mandate
          .withStatus(Mandate.MandateStatus.MANDATE_SUBMITTED)
      case "canceled" =>
        mandate = mandate
          .withStatus(Mandate.MandateStatus.MANDATE_EXPIRED)
          .withResultMessage("the confirmation limit has been reached")
      case _ =>
        mandate = mandate
          .withStatus(Mandate.MandateStatus.MANDATE_FAILED)
          .withResultMessage("the mandate has failed")
    }
    mandate
  }

}
