package app.softnetwork.payment.service

import app.softnetwork.payment.handlers.GenericPaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model._
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.Part
import sttp.tapir.json.json4s.jsonBody
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ServerEndpoint.Full

import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}

trait GenericPaymentEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends RootPaymentEndpoints[SD]
    with CardEndpoints[SD]
    with CardPaymentEndpoints[SD]
    with BankAccountEndpoints[SD]
    with KycDocumentEndpoints[SD]
    with UboDeclarationEndpoints[SD]
    with RecurringPaymentEndpoints[SD]
    with MandateEndpoints[SD] {
  _: GenericPaymentHandler with SessionMaterials[SD] =>

  import app.softnetwork.serialization._

  /** should be implemented by each payment provider
    */
  def hooks: Full[Unit, Unit, (String, String), Unit, Unit, Any, Future]

  val loadPaymentAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    secureEndpoint.get
      .out(jsonBody[PaymentAccountView].description("Authenticated user payment account"))
      .serverLogic(session =>
        _ => {
          run(LoadPaymentAccount(externalUuidWithProfile(session))).map {
            case r: PaymentAccountLoaded => Right(r.paymentAccount.view)
            case other                   => Left(error(other))
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

}

case class UploadDocument(pages: Part[Array[Byte]]) { //TODO pages may include multiple parts
  lazy val bytes: Seq[Array[Byte]] = Seq(pages.body)
}
