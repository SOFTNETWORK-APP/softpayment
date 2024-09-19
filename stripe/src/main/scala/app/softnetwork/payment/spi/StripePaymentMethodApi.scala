package app.softnetwork.payment.spi

import app.softnetwork.payment.config.StripeApi
import app.softnetwork.payment.model.{Card, PaymentMethod, Paypal, PreRegistration, Transaction}
import com.google.gson.Gson
import com.stripe.model.{Customer, PaymentMethod => StripePaymentMethod, SetupIntent}
import com.stripe.param.{
  PaymentMethodAttachParams,
  PaymentMethodDetachParams,
  SetupIntentCreateParams
}

import scala.util.{Failure, Success, Try}

trait StripePaymentMethodApi extends PaymentMethodApi { _: StripeContext =>

  /** @param maybeUserId
    *   - owner of the payment method
    * @param currency
    *   - currency
    * @param externalUuid
    *   - external unique id
    * @param paymentType
    *   - payment type
    * @return
    *   pre registration
    */
  override def preRegisterPaymentMethod(
    maybeUserId: Option[String],
    currency: String,
    externalUuid: String,
    paymentType: Transaction.PaymentType
  ): Option[PreRegistration] = {
    maybeUserId match {
      case Some(userId) if paymentType.isCard || paymentType.isPaypal =>
        Try {
          val customer =
            Customer.retrieve(userId, StripeApi().requestOptions)

          val params =
            SetupIntentCreateParams
              .builder()
              .addPaymentMethodType(paymentType.name.toLowerCase)
              .setCustomer(customer.getId)
              .setUsage(SetupIntentCreateParams.Usage.OFF_SESSION)
              //              .setUseStripeSdk(false)
              //              .addFlowDirection(SetupIntentCreateParams.FlowDirection.INBOUND)
              .putMetadata("currency", currency)
              .putMetadata("external_uuid", externalUuid)
          // TODO check if return url is required

          paymentType match {
            case Transaction.PaymentType.CARD =>
              params
                .setPaymentMethodOptions(
                  SetupIntentCreateParams.PaymentMethodOptions
                    .builder()
                    .setCard(
                      SetupIntentCreateParams.PaymentMethodOptions.Card
                        .builder()
                        .setRequestThreeDSecure(
                          SetupIntentCreateParams.PaymentMethodOptions.Card.RequestThreeDSecure.ANY
                        )
                        .build()
                    )
                    .build()
                )
            case _ =>
          }

          SetupIntent.create(params.build(), StripeApi().requestOptions)
        } match {
          case Success(setupIntent) =>
            mlog.info(s"Card pre registered for user $userId -> ${new Gson().toJson(setupIntent)}")
            Some(
              PreRegistration.defaultInstance
                .withId(setupIntent.getId)
                .withSecretKey(setupIntent.getClientSecret)
            )
          case Failure(f) =>
            mlog.error(s"Error while pre registering card for user $userId", f)
            None
        }
      case Some(_) =>
        mlog.error(s"Unsupported payment type $paymentType")
        None
      case _ => None
    }
  }

  /** @param registrationId
    *   - payment method registration id
    * @param maybeRegistrationData
    *   - optional registration data
    * @return
    *   payment method id
    */
  override def registerPaymentMethod(
    registrationId: String,
    maybeRegistrationData: Option[String]
  ): Option[String] = {
    Try {
      // retrieve setup intent
      val setupIntent =
        SetupIntent.retrieve(registrationId, StripeApi().requestOptions)
      // attach payment method to customer
      attachPaymentMethod(setupIntent.getPaymentMethod, setupIntent.getCustomer)
    } match {
      case Success(value) =>
        value match {
          case Some(pm) => Some(pm.id)
          case None     => None
        }
      case Failure(f) =>
        mlog.error(s"Error while registering payment method for $registrationId", f)
        None
    }
  }

  /** @param paymentMethodId
    *   - payment method id
    * @return
    *   payment method or none
    */
  override def loadPaymentMethod(paymentMethodId: String): Option[PaymentMethod] = {
    Try {
      StripePaymentMethod.retrieve(paymentMethodId, StripeApi().requestOptions)
    } match {
      case Success(paymentMethod) =>
        paymentMethod.getType match {
          case "card" =>
            Option(paymentMethod.getCard) match {
              case Some(card) =>
                Some(
                  Card.defaultInstance
                    .withId(paymentMethodId)
                    .withExpirationDate(s"${card.getExpMonth}/${card.getExpYear}")
                    .withAlias(card.getLast4)
                    .withBrand(card.getBrand)
                    .withActive(
                      Option(paymentMethod.getCustomer).isDefined
                    ) // if detached from customer, it is disabled
                )
              case _ => None
            }
          case "paypal" =>
            Option(paymentMethod.getPaypal) match {
              case Some(paypal) =>
                Some(
                  Paypal.defaultInstance
                    .withId(paymentMethodId)
                    .withPayerId(paypal.getPayerId)
                    .withActive(
                      Option(paymentMethod.getCustomer).isDefined
                    ) // if detached from customer, it is disabled
                    .copy(
                      payerEmail = Option(paypal.getPayerEmail)
                    )
                )
              case _ => None
            }
          case _ =>
            mlog.error(s"Unsupported payment method type ${paymentMethod.getType}")
            None
        }
      case Failure(f) =>
        mlog.error(s"Error while loading card $paymentMethodId", f)
        None
    }
  }

  /** attach a payment method to a user
    *
    * @param paymentMethodId
    *   - payment method id to register
    * @param userId
    *   - owner of the payment method
    * @return
    *   payment method attached
    */
  override def attachPaymentMethod(
    paymentMethodId: String,
    userId: String
  ): Option[PaymentMethod] = {
    Try {
      val requestOptions = StripeApi().requestOptions
      StripePaymentMethod
        .retrieve(paymentMethodId, requestOptions)
        .attach(
          PaymentMethodAttachParams.builder().setCustomer(userId).build(),
          requestOptions
        )
    } match {
      case Success(paymentMethod) =>
        paymentMethod.getType match {
          case "card" =>
            mlog.info(s"Card ${paymentMethod.getId} attached to customer $userId")
            Option(paymentMethod.getCard) match {
              case Some(card) =>
                Some(
                  Card.defaultInstance
                    .withId(paymentMethod.getId)
                    .withExpirationDate(s"${card.getExpMonth}/${card.getExpYear}")
                    .withAlias(card.getLast4)
                    .withBrand(card.getBrand)
                    .withFingerprint(card.getFingerprint)
                    .withActive(
                      Option(paymentMethod.getCustomer).isDefined
                    ) // if detached from customer, it is disabled
                )
              case _ => None
            }
          case "paypal" =>
            mlog.info(s"Paypal ${paymentMethod.getId} attached to customer $userId")
            Option(paymentMethod.getPaypal) match {
              case Some(paypal) =>
                Some(
                  Paypal.defaultInstance
                    .withId(paymentMethod.getId)
                    .withPayerId(paypal.getPayerId)
                    .withActive(
                      Option(paymentMethod.getCustomer).isDefined
                    ) // if detached from customer, it is disabled
                    .copy(
                      payerEmail = Option(paypal.getPayerEmail)
                    )
                )
              case _ => None
            }
          case _ =>
            mlog.error(s"Unsupported payment method type ${paymentMethod.getType}")
            None
        }
      case Failure(f) =>
        mlog.error(
          s"Error while attaching payment method $paymentMethodId to customer $userId",
          f
        )
        None
    }
  }

  /** Disable a means of payment
    *
    * @param paymentMethodId
    *   - means of payment to disable
    */
  override def disablePaymentMethod(paymentMethodId: String): Option[PaymentMethod] = {
    Try {
      val requestOptions = StripeApi().requestOptions
      StripePaymentMethod
        .retrieve(paymentMethodId, requestOptions)
        .detach(PaymentMethodDetachParams.builder().build(), requestOptions)
    } match {
      case Success(pm) =>
        pm.getType match {
          case "card" =>
            mlog.info(
              s"Card ${pm.getId} detached from customer"
            )
            Option(pm.getCard) match {
              case Some(card) =>
                Some(
                  Card.defaultInstance
                    .withId(pm.getId)
                    .withExpirationDate(s"${card.getExpMonth}/${card.getExpYear}")
                    .withAlias(card.getLast4)
                    .withBrand(card.getBrand)
                    .withFingerprint(card.getFingerprint)
                    .withActive(false)
                )
              case _ => None
            }
          case "paypal" =>
            mlog.info(
              s"Paypal ${pm.getId} detached from customer ${pm.getCustomer}"
            )
            Option(pm.getPaypal) match {
              case Some(paypal) =>
                Some(
                  Paypal.defaultInstance
                    .withId(pm.getId)
                    .withPayerId(paypal.getPayerId)
                    .withActive(false)
                    .copy(
                      payerEmail = Option(paypal.getPayerEmail)
                    )
                )
              case _ => None
            }
          case _ =>
            mlog.error(s"Unsupported means of payment type ${pm.getType}")
            None
        }
      case Failure(f) =>
        mlog.error(s"Error while disabling payment method ${paymentMethodId}", f)
        None
    }
  }
}
