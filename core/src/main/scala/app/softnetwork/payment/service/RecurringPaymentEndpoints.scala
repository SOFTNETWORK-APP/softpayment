package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.{RecurringPayment, RecurringPaymentView}
import org.softnetwork.session.model.Session
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.{Method, StatusCode}
import sttp.model.headers.CookieValueWithMeta
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait RecurringPaymentEndpoints extends BasicPaymentService {
  _: GenericPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  val registerRecurringPayment: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    RegisterRecurringPayment,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], PaymentResult),
    Any,
    Future
  ] =
    secureEndpoint.post
      .in(PaymentSettings.RecurringPaymentRoute)
      .in(jsonBody[RegisterRecurringPayment].description("Recurring payment to register"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[RecurringPaymentRegistered](
            statusCode(StatusCode.Ok).and(
              jsonBody[RecurringPaymentRegistered].description(
                "Recurring payment successfully registered"
              )
            )
          ),
          oneOfVariant[MandateConfirmationRequired](
            statusCode(StatusCode.Ok).and(
              jsonBody[MandateConfirmationRequired].description(
                "Recurring payment registration required a mandate confirmation"
              )
            )
          ),
          oneOfVariant[PaymentErrorMessage](
            statusCode(StatusCode.BadRequest)
              .and(
                jsonBody[PaymentErrorMessage].description("Recurring payment registration failure")
              )
          ),
          oneOfVariant[PaymentAccountNotFound.type](
            statusCode(StatusCode.NotFound)
              .and(jsonBody[PaymentAccountNotFound.type].description("Payment account not found"))
          )
        )
      )
      .serverLogic(principal =>
        cmd =>
          run(cmd.copy(debitedAccount = externalUuidWithProfile(principal._2))).map {
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, PaymentAccountNotFound))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, PaymentErrorMessage(r.message)))
            case other => Right((principal._1._1, principal._1._2, other))
          }
      )
      .description("Register a recurring payment for the authenticated payment account")

  val loadRecurringPayment: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    String,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], Either[PaymentResult, RecurringPaymentView]),
    Any,
    Future
  ] =
    secureEndpoint.get
      .in(PaymentSettings.RecurringPaymentRoute)
      .in(path[String])
      .out(
        oneOf[Either[PaymentResult, RecurringPaymentView]](
          oneOfVariantValueMatcher[Right[PaymentResult, RecurringPaymentView]](
            statusCode(StatusCode.Ok).and(
              jsonBody[Right[PaymentResult, RecurringPaymentView]]
                .description("Recurring payment successfully loaded")
            )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[RecurringPaymentView](
            "Recurring payment loading failure"
          ),
          oneOfVariantValueMatcher[Left[PaymentAccountNotFound.type, RecurringPaymentView]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, RecurringPaymentView]]
                  .description("Payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          },
          oneOfVariantValueMatcher[Left[RecurringPaymentNotFound.type, RecurringPaymentView]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[RecurringPaymentNotFound.type, RecurringPaymentView]]
                  .description("Recurring payment not found for the authenticated payment account")
              )
          ) { case Left(RecurringPaymentNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        recurringPaymentRegistrationId =>
          run(
            LoadRecurringPayment(
              externalUuidWithProfile(principal._2),
              recurringPaymentRegistrationId
            )
          ).map {
            case r: RecurringPaymentLoaded =>
              Right((principal._1._1, principal._1._2, Right(r.recurringPayment.view)))
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, Left(PaymentAccountNotFound)))
            case RecurringPaymentNotFound =>
              Right((principal._1._1, principal._1._2, Left(RecurringPaymentNotFound)))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
          }
      )
      .description("Load the recurring payment of the authenticated payment account")

  val updateRecurringCardPaymentRegistration: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    UpdateRecurringCardPaymentRegistration,
    PaymentError,
    (
      Seq[Option[String]],
      Option[CookieValueWithMeta],
      Either[PaymentResult, RecurringPayment.RecurringCardPaymentResult]
    ),
    Any,
    Future
  ] =
    secureEndpoint.put
      .in(PaymentSettings.RecurringPaymentRoute)
      .in(
        jsonBody[UpdateRecurringCardPaymentRegistration].description(
          "Recurring card payment update"
        )
      )
      .out(
        oneOf[Either[PaymentResult, RecurringPayment.RecurringCardPaymentResult]](
          oneOfVariantValueMatcher[
            Right[PaymentResult, RecurringPayment.RecurringCardPaymentResult]
          ](
            statusCode(StatusCode.Ok)
              .and(
                jsonBody[Right[PaymentResult, RecurringPayment.RecurringCardPaymentResult]]
                  .description("Recurring card payment successfully updated")
              )
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[RecurringPayment.RecurringCardPaymentResult](
            "Recurring card payment update failure"
          ),
          oneOfVariantValueMatcher[
            Left[PaymentAccountNotFound.type, RecurringPayment.RecurringCardPaymentResult]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[
                  Left[PaymentAccountNotFound.type, RecurringPayment.RecurringCardPaymentResult]
                ].description("Payment account not found")
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          },
          oneOfVariantValueMatcher[
            Left[RecurringPaymentNotFound.type, RecurringPayment.RecurringCardPaymentResult]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[
                  Left[RecurringPaymentNotFound.type, RecurringPayment.RecurringCardPaymentResult]
                ].description("Recurring payment not found")
              )
          ) { case Left(RecurringPaymentNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        cmd =>
          run(cmd.copy(debitedAccount = externalUuidWithProfile(principal._2))).map {
            case r: RecurringCardPaymentRegistrationUpdated =>
              Right((principal._1._1, principal._1._2, Right(r.result)))
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, Left(PaymentAccountNotFound)))
            case RecurringPaymentNotFound =>
              Right((principal._1._1, principal._1._2, Left(RecurringPaymentNotFound)))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
          }
      )
      .description(
        "Update recurring card payment registration of the authenticated payment account"
      )

  val deleteRecurringPayment: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    String,
    PaymentError,
    (
      Seq[Option[String]],
      Option[CookieValueWithMeta],
      Either[PaymentResult, RecurringPayment.RecurringCardPaymentResult]
    ),
    Any,
    Future
  ] =
    secureEndpoint.delete
      .in(PaymentSettings.RecurringPaymentRoute)
      .in(path[String])
      .out(
        oneOf[Either[PaymentResult, RecurringPayment.RecurringCardPaymentResult]](
          oneOfVariantValueMatcher[
            Right[PaymentResult, RecurringPayment.RecurringCardPaymentResult]
          ](
            statusCode(StatusCode.Ok)
              .and(jsonBody[Right[PaymentResult, RecurringPayment.RecurringCardPaymentResult]])
          ) { case Right(_) =>
            true
          },
          oneOfPaymentErrorMessageValueMatcher[RecurringPayment.RecurringCardPaymentResult](
            "Recurring payment deletion failure"
          ),
          oneOfVariantValueMatcher[
            Left[PaymentAccountNotFound.type, RecurringPayment.RecurringCardPaymentResult]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[
                  Left[PaymentAccountNotFound.type, RecurringPayment.RecurringCardPaymentResult]
                ]
              )
          ) { case Left(PaymentAccountNotFound) =>
            true
          },
          oneOfVariantValueMatcher[
            Left[RecurringPaymentNotFound.type, RecurringPayment.RecurringCardPaymentResult]
          ](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[
                  Left[RecurringPaymentNotFound.type, RecurringPayment.RecurringCardPaymentResult]
                ]
              )
          ) { case Left(RecurringPaymentNotFound) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        recurringPaymentRegistrationId =>
          run(
            UpdateRecurringCardPaymentRegistration(
              externalUuidWithProfile(principal._2),
              recurringPaymentRegistrationId,
              None,
              Some(RecurringPayment.RecurringCardPaymentStatus.ENDED)
            )
          ).map {
            case r: RecurringCardPaymentRegistrationUpdated =>
              Right((principal._1._1, principal._1._2, Right(r.result)))
            case PaymentAccountNotFound =>
              Right((principal._1._1, principal._1._2, Left(PaymentAccountNotFound)))
            case RecurringPaymentNotFound =>
              Right((principal._1._1, principal._1._2, Left(RecurringPaymentNotFound)))
            case r: PaymentError =>
              Right((principal._1._1, principal._1._2, Left(PaymentErrorMessage(r.message))))
          }
      )

  val recurringPaymentEndpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      registerRecurringPayment,
      updateRecurringCardPaymentRegistration,
      loadRecurringPayment,
      deleteRecurringPayment
    )
}
