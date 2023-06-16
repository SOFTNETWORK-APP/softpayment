package app.softnetwork.payment.service

import akka.http.scaladsl.server.Route
import app.softnetwork.api.server.ApiEndpoint
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model._
import org.softnetwork.session.model.Session
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.headers.CookieValueWithMeta
import sttp.model.{Method, Part, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir._

import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}

trait GenericPaymentEndpoints
    extends RootPaymentEndpoints
    with CardEndpoints
    with CardPaymentEndpoints
    with BankAccountEndpoints
    with KycDocumentEndpoints
    with UboDeclarationEndpoints
    with RecurringPaymentEndpoints
    with MandateEndpoints
    with ApiEndpoint {
  _: GenericPaymentHandler =>

  import app.softnetwork.serialization._

  /** should be implemented by each payment provider
    */
  def hooks: Full[Unit, Unit, (String, String), Unit, Unit, Any, Future]

  val loadPaymentAccount: Full[
    (Seq[Option[String]], Method, Option[String], Option[String]),
    ((Seq[Option[String]], Option[CookieValueWithMeta]), Session),
    Unit,
    PaymentError,
    (Seq[Option[String]], Option[CookieValueWithMeta], Either[PaymentResult, PaymentAccountView]),
    Any,
    Future
  ] =
    secureEndpoint.get
      .out(
        oneOf[Either[PaymentResult, PaymentAccountView]](
          oneOfVariantValueMatcher[Right[PaymentResult, PaymentAccountView]](
            statusCode(StatusCode.Ok).and(
              jsonBody[Right[PaymentResult, PaymentAccountView]]
                .description("Authenticated user payment account")
            )
          ) { case Right(_) =>
            true
          },
          oneOfVariantValueMatcher[Left[PaymentAccountNotFound.type, PaymentAccountView]](
            statusCode(StatusCode.NotFound)
              .and(
                jsonBody[Left[PaymentAccountNotFound.type, PaymentAccountView]]
                  .description("Payment account not found")
              )
          ) { case Left(CardNotDisabled) =>
            true
          }
        )
      )
      .serverLogic(principal =>
        _ => {
          run(LoadPaymentAccount(externalUuidWithProfile(principal._2))).map {
            case r: PaymentAccountLoaded =>
              Right((principal._1._1, principal._1._2, Right(r.paymentAccount.view)))
            case other => Right((principal._1._1, principal._1._2, Left(other)))
          }
        }
      )
      .description("Load authenticated user payment account")

  override val endpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    cardEndpoints ++
    cardPaymentEndpoints ++
    bankAccountEndpoints ++
    kycDocumentEndpoints ++
    uboDeclarationEndpoints ++
    mandateEndpoints ++
    recurringPaymentEndpoints ++
    List(
      loadPaymentAccount,
      hooks
    )

  lazy val route: Route = apiRoute
}

case class UploadDocument(pages: Part[Array[Byte]]) { //TODO pages may include multiple parts
  lazy val bytes: Seq[Array[Byte]] = Seq(pages.body)
}
