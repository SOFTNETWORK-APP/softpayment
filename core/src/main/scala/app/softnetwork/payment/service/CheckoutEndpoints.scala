package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.headers.CookieValueWithMeta
import sttp.model.{HeaderNames, Method, StatusCode}
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.{PartialServerEndpointWithSecurityOutput, ServerEndpoint}

import scala.concurrent.Future

trait CheckoutEndpoints[SD <: SessionData with SessionDataDecorator[SD]] {
  _: RootPaymentEndpoints[SD] with PaymentHandler =>

  import app.softnetwork.serialization._

  def payment(payment: Payment): PartialServerEndpointWithSecurityOutput[
    (Seq[Option[String]], Option[String], Method, Option[String]),
    (Option[SoftPayAccount.Client], SD),
    (Option[String], Option[String], Option[String], Option[String], Payment),
    Any,
    (Seq[Option[String]], Option[CookieValueWithMeta]),
    Unit,
    Any,
    Future
  ] =
    requiredSessionEndpoint
      .in(header[Option[String]](HeaderNames.AcceptLanguage))
      .in(header[Option[String]](HeaderNames.Accept))
      .in(header[Option[String]](HeaderNames.UserAgent))
      .in(clientIp)
      .in(
        jsonBody[Payment]
          .description("Payment to perform")
          .example(payment)
      )

  val preAuthorize: ServerEndpoint[Any with AkkaStreams, Future] =
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
      .in(PaymentSettings.PaymentConfig.preAuthorizeRoute)
      .in(paths.description("optional credited account"))
      .post
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaymentPreAuthorized](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaymentPreAuthorized].description("Card pre authorized transaction id"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(
                jsonBody[PaymentRedirection].description(
                  "Pre authorization redirection to 3D secure"
                )
              )
          ),
          oneOfVariant[PaymentRequired](
            statusCode(StatusCode.PaymentRequired)
              .and(jsonBody[PaymentRequired].description("Payment required"))
          )
        )
      )
      .serverLogic(principal => {
        case (language, accept, userAgent, ipAddress, payment, creditedAccount) =>
          val browserInfo = extractBrowserInfo(language, accept, userAgent, payment)
          import payment._
          run(
            PreAuthorize(
              orderUuid,
              externalUuidWithProfile(principal._2),
              debitedAmount,
              currency,
              registrationId,
              registrationData,
              registerCard,
              if (browserInfo.isDefined) ipAddress else None,
              browserInfo,
              printReceipt,
              creditedAccount.headOption,
              feesAmount,
              user = user, // required for Pre authorize without pre registered card
              paymentMethodId = paymentMethodId,
              registerMeansOfPayment = registerMeansOfPayment
            )
          ).map {
            case result: PaymentPreAuthorized => Right(result)
            case result: PaymentRedirection   => Right(result)
            case result: PaymentRequired      => Right(result)
            case other                        => Left(error(other))
          }
      })
      .description("Pre authorize card")

  val preAuthorizeCallback: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(
        PaymentSettings.PaymentConfig.callbacksRoute / PaymentSettings.PaymentConfig.preAuthorizeRoute
      )
      .in(path[String].description("Order uuid"))
      .in(queryParams)
      .description("Pre authorization query parameters")
      /*.in(query[String]("preAuthorizationId").description("pre authorized transaction id"))
      .in(
        query[Boolean]("registerMeansOfPayment").description(
          "Whether to register or not the payment method after pre authorization"
        )
      )
      .in(query[Boolean]("printReceipt").description("Whether or not a receipt should be printed"))*/
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaymentPreAuthorized](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaymentPreAuthorized].description("Card pre authorized transaction Id"))
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
      .serverLogic { case (orderUuid, params) =>
        val preAuthorizationIdParameter =
          params.get("preAuthorizationIdParameter").getOrElse("preAuthorizationId")
        val preAuthorizationId = params.get(preAuthorizationIdParameter).getOrElse("")
        val registerMeansOfPayment =
          params.get("registerMeansOfPayment").getOrElse("false").toBoolean
        val printReceipt = params.get("printReceipt").getOrElse("false").toBoolean
        run(
          PreAuthorizeCallback(orderUuid, preAuthorizationId, registerMeansOfPayment, printReceipt)
        ).map {
          case result: PaymentPreAuthorized => Right(result)
          case result: PaymentRedirection   => Right(result)
          case other                        => Left(error(other))
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
      .in(PaymentSettings.PaymentConfig.payInRoute)
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
              .and(jsonBody[PaymentRedirection].description("Payment redirection"))
          ),
          oneOfVariant[PaymentRequired](
            statusCode(StatusCode.PaymentRequired)
              .and(jsonBody[PaymentRequired].description("Payment required"))
          )
        )
      )
      .serverLogic(principal => {
        case (language, accept, userAgent, ipAddress, payment, creditedAccount) =>
          val browserInfo = extractBrowserInfo(language, accept, userAgent, payment)
          import payment._
          run(
            PayIn(
              orderUuid,
              externalUuidWithProfile(principal._2),
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
              printReceipt,
              feesAmount,
              user = user, // required for Pay in without registered card (eg PayPal)
              registerMeansOfPayment = registerMeansOfPayment,
              paymentMethodId = paymentMethodId,
              clientId = principal._1.map(_.clientId).orElse(principal._2.clientId)
            )
          ).map {
            case result: PaidIn             => Right(result)
            case result: PaymentRedirection => Right(result)
            case result: PaymentRequired    => Right(result)
            case other                      => Left(error(other))
          }
      })
      .description("Pay in")

  val payInCallback: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(PaymentSettings.PaymentConfig.callbacksRoute / PaymentSettings.PaymentConfig.payInRoute)
      .in(path[String].description("Order uuid"))
      .in(queryParams)
      .description("Pay in query parameters")
      /*.in(query[String]("transactionId").description("Payment transaction id"))
      .in(query[Boolean]("printReceipt").description("Whether or not a receipt should be printed"))*/
      .out(
        oneOf[PaymentResult](
          oneOfVariant[PaidIn](
            statusCode(StatusCode.Ok)
              .and(jsonBody[PaidIn].description("Payment transaction result"))
          ),
          oneOfVariant[PaymentRedirection](
            statusCode(StatusCode.Accepted)
              .and(jsonBody[PaymentRedirection].description("Payment redirection to 3D secure"))
          ),
          oneOfVariant[PaymentRequired](
            statusCode(StatusCode.PaymentRequired)
              .and(jsonBody[PaymentRequired].description("Payment required"))
          )
        )
      )
      .description("Pay in with card")
      .serverLogic { case (orderUuid, params) =>
        val transactionIdParameter =
          params.get("transactionIdParameter").getOrElse("transactionId")
        val transactionId = params.get(transactionIdParameter).getOrElse("")
        val registerMeansOfPayment =
          params.get("registerMeansOfPayment").getOrElse("false").toBoolean
        val printReceipt = params.get("printReceipt").getOrElse("false").toBoolean
        run(
          PayInCallback(orderUuid, transactionId, registerMeansOfPayment, printReceipt)
        ).map {
          case result: PaidIn             => Right(result)
          case result: PaymentRedirection => Right(result)
          case result: PaymentRequired    => Right(result)
          case other                      => Left(error(other))
        }
      }

  val executeFirstRecurringCardPayment: ServerEndpoint[Any with AkkaStreams, Future] =
    payment(Payment("", 0)).post
      .in(PaymentSettings.PaymentConfig.recurringPaymentRoute)
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
      .serverLogic(principal => {
        case (language, accept, userAgent, ipAddress, payment, recurringPaymentRegistrationId) =>
          val browserInfo = extractBrowserInfo(language, accept, userAgent, payment)
          import payment._
          run(
            ExecuteFirstRecurringPayment(
              recurringPaymentRegistrationId,
              externalUuidWithProfile(principal._2),
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

  val executeFirstRecurringCardPaymentCallback: ServerEndpoint[Any with AkkaStreams, Future] =
    rootEndpoint
      .in(
        PaymentSettings.PaymentConfig.callbacksRoute / PaymentSettings.PaymentConfig.recurringPaymentRoute
      )
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
          FirstRecurringPaymentCallback(recurringPayInRegistrationId, transactionId)
        ).map {
          case result: PaidIn             => Right(result)
          case result: PaymentRedirection => Right(result)
          case other                      => Left(error(other))
        }
      }

  val cardPaymentEndpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      preAuthorize,
      preAuthorizeCallback,
      payIn,
      payInCallback,
      executeFirstRecurringCardPayment,
      executeFirstRecurringCardPaymentCallback
    )
}
