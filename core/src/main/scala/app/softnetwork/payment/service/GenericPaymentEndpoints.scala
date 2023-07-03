package app.softnetwork.payment.service

import akka.http.scaladsl.server.Route
import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model._
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.Part
import sttp.tapir.generic.auto._
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ServerEndpoint.Full

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
    with MandateEndpoints {
  _: GenericPaymentHandler =>

  import app.softnetwork.serialization._

  /** should be implemented by each payment provider
    */
  def hooks: Full[Unit, Unit, (String, String), Unit, Unit, Any, Future]

  val loadPaymentAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.get
      .out(jsonBody[PaymentAccountView].description("Authenticated user payment account"))
      .serverLogic(principal =>
        _ => {
          run(LoadPaymentAccount(externalUuidWithProfile(principal._2))).map {
            case r: PaymentAccountLoaded =>
              Right((principal._1._1, principal._1._2, r.paymentAccount.view))
            case other => Left(error(other))
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
