package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import org.softnetwork.session.model.Session
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.{HeaderNames, Method, StatusCode}
import sttp.model.headers.CookieValueWithMeta
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.{PartialServerEndpointWithSecurityOutput, ServerEndpoint}

import scala.concurrent.Future

trait CardPaymentEndpoints { _: RootPaymentEndpoints with GenericPaymentHandler =>

  import app.softnetwork.serialization._

  def payment(payment: Payment): PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    Session,
    (Option[String], Option[String], Option[String], Option[String], Payment),
    Any,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Unit,
    Any,
    Future
  ] =
    secureEndpoint
      .in(header[Option[String]](HeaderNames.AcceptLanguage))
      .in(header[Option[String]](HeaderNames.Accept))
      .in(header[Option[String]](HeaderNames.UserAgent))
      .in(clientIp)
      .in(
        jsonBody[Payment]
          .description("Payment to perform")
          .example(payment)
      )

  val preAuthorizeCard: ServerEndpoint[Any with AkkaStreams, Future] =
    payment(
      Payment(
        "pre-authorize-order-96",
        5100,
        registrationId = Some("2992bf00-8d3e-4448-8737-d7362b144de5"),
        registrationData = Some("data"),
        registerCard = true,
        printReceipt = true
      )
    )
      .in(PaymentSettings.PreAuthorizeCardRoute)
      .post
      .out(
        oneOf[PaymentResult](
          oneOfVariant[CardPreAuthorized](
            statusCode(StatusCode.Ok)
              .and(jsonBody[CardPreAuthorized].description("Card pre authorization transaction id"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(
                jsonBody[PaymentRedirection].description(
                  "Pre authorization redirection to 3D secure"
                )
              )
          )
        )
      )
      .serverLogic(session => { case (language, accept, userAgent, ipAddress, payment) =>
        val browserInfo = extractBrowserInfo(language, accept, userAgent, payment)
        import payment._
        run(
          PreAuthorizeCard(
            orderUuid,
            externalUuidWithProfile(session),
            debitedAmount,
            currency,
            registrationId,
            registrationData,
            registerCard,
            if (browserInfo.isDefined) ipAddress else None,
            browserInfo,
            printReceipt
          )
        ).map {
          case result: CardPreAuthorized  => Right(result)
          case result: PaymentRedirection => Right(result)
          case other                      => Left(error(other))
        }
      })
      .description("Pre authorize card")

  val preAuthorizeCardFor3DS: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.SecureModeRoute / PaymentSettings.PreAuthorizeCardRoute)
      .in(path[String].description("Order uuid"))
      .in(query[String]("preAuthorizationId").description("Pre authorization transaction id"))
      .in(
        query[Boolean]("registerCard").description(
          "Whether to register or not the card after successfully pre authorization"
        )
      )
      .in(query[Boolean]("printReceipt").description("Whether or not a receipt should be printed"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[CardPreAuthorized](
            statusCode(StatusCode.Ok)
              .and(jsonBody[CardPreAuthorized].description("Card pre authorization transaction Id"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(
                jsonBody[PaymentRedirection].description(
                  "Pre authorization redirection to 3D secure"
                )
              )
          )
        )
      )
      .description("Pre authorize card for 3D secure")
      .serverLogic { case (orderUuid, preAuthorizationId, registerCard, printReceipt) =>
        run(
          PreAuthorizeCardFor3DS(orderUuid, preAuthorizationId, registerCard, printReceipt)
        ).map {
          case result: CardPreAuthorized  => Right(result)
          case result: PaymentRedirection => Right(result)
          case other                      => Left(error(other))
        }
      }

  val payIn: ServerEndpoint[Any with AkkaStreams, Future] =
    payment(
      Payment(
        "pay-in-order-97",
        5100,
        registrationId = Some("2992bf00-8d3e-4448-8737-d7362b144de5"),
        registrationData = Some("data"),
        registerCard = true,
        printReceipt = true
      )
    )
      .in(PaymentSettings.PayInRoute)
      .in(path[String].description("credited account"))
      .post
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaidIn](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaidIn].description("Payment transaction result"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(jsonBody[PaymentRedirection].description("Payment redirection to 3D secure"))
          )
        )
      )
      .serverLogic(session => {
        case (language, accept, userAgent, ipAddress, payment, creditedAccount) =>
          val browserInfo = extractBrowserInfo(language, accept, userAgent, payment)
          import payment._
          run(
            PayIn(
              orderUuid,
              externalUuidWithProfile(session),
              debitedAmount,
              currency,
              creditedAccount,
              registrationId,
              registrationData,
              registerCard,
              if (browserInfo.isDefined) Some(ipAddress) else None,
              browserInfo,
              statementDescriptor,
              paymentType,
              printReceipt
            )
          ).map {
            case result: PaidIn             => Right(result)
            case result: PaymentRedirection => Right(result)
            case other                      => Left(error(other))
          }
      })
      .description("Pay in")

  val payInFor3DS: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.SecureModeRoute / PaymentSettings.PayInRoute)
      .in(path[String].description("Order uuid"))
      .in(query[String]("transactionId").description("Payment transaction id"))
      .in(
        query[Boolean]("registerCard").description(
          "Whether to register or not the card after successfully pay in"
        )
      )
      .in(query[Boolean]("printReceipt").description("Whether or not a receipt should be printed"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaidIn](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaidIn].description("Payment transaction result"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(jsonBody[PaymentRedirection].description("Payment redirection to 3D secure"))
          )
        )
      )
      .description("Pay in for 3D secure")
      .serverLogic { case (orderUuid, transactionId, registerCard, printReceipt) =>
        run(
          PayInFor3DS(orderUuid, transactionId, registerCard, printReceipt)
        ).map {
          case result: PaidIn             => Right(result)
          case result: PaymentRedirection => Right(result)
          case other                      => Left(error(other))
        }
      }

  val payInForPayPal: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.PayPalRoute)
      .in(path[String].description("Order uuid"))
      .in(query[String]("transactionId").description("Payment transaction id"))
      .in(query[Boolean]("printReceipt").description("Whether or not a receipt should be printed"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaidIn](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaidIn].description("Payment transaction result"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(jsonBody[PaymentRedirection].description("Payment redirection to 3D secure"))
          )
        )
      )
      .description("Pay in for PayPal")
      .serverLogic { case (orderUuid, transactionId, printReceipt) =>
        run(
          PayInForPayPal(orderUuid, transactionId, printReceipt)
        ).map {
          case result: PaidIn             => Right(result)
          case result: PaymentRedirection => Right(result)
          case other                      => Left(error(other))
        }
      }

  val executeFirstRecurringCardPayment: ServerEndpoint[Any with AkkaStreams, Future] =
    payment(Payment("", 0)).post
      .in(PaymentSettings.RecurringPaymentRoute)
      .in(path[String].description("Recurring payment registration Id"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[FirstRecurringPaidIn](
            statusCode(StatusCode.Ok).and(
              jsonBody[FirstRecurringPaidIn]
                .description("First recurring payment transaction result")
            )
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(
                jsonBody[PaymentRedirection].description(
                  "First recurring payment redirection to 3D secure"
                )
              )
          )
        )
      )
      .serverLogic(session => {
        case (language, accept, userAgent, ipAddress, payment, recurringPaymentRegistrationId) =>
          val browserInfo = extractBrowserInfo(language, accept, userAgent, payment)
          import payment._
          run(
            PayInFirstRecurring(
              recurringPaymentRegistrationId,
              externalUuidWithProfile(session),
              if (browserInfo.isDefined) Some(ipAddress) else None,
              browserInfo,
              statementDescriptor
            )
          ).map {
            case result: FirstRecurringPaidIn => Right(result)
            case result: PaymentRedirection   => Right(result)
            case other                        => Left(error(other))
          }
      })
      .description("Execute first recurring payment")

  val executeFirstRecurringCardPaymentFor3DS: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.SecureModeRoute / PaymentSettings.RecurringPaymentRoute)
      .in(path[String].description("Recurring payment registration Id"))
      .in(query[String]("transactionId").description("First recurring payment transaction Id"))
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaidIn](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaidIn].description("First recurring payment transaction result"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(
                jsonBody[PaymentRedirection].description(
                  "First recurring payment redirection to 3D secure"
                )
              )
          )
        )
      )
      .description("Execute first recurring payment for 3D secure")
      .serverLogic { case (recurringPayInRegistrationId, transactionId) =>
        run(
          PayInFirstRecurringFor3DS(recurringPayInRegistrationId, transactionId)
        ).map {
          case result: PaidIn             => Right(result)
          case result: PaymentRedirection => Right(result)
          case other                      => Left(error(other))
        }
      }

  val cardPaymentEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      preAuthorizeCard,
      preAuthorizeCardFor3DS,
      payIn,
      payInFor3DS,
      payInForPayPal,
      executeFirstRecurringCardPayment,
      executeFirstRecurringCardPaymentFor3DS
    )
}
