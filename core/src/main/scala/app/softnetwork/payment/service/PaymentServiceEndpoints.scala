package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.message.PaymentMessages._
import app.softnetwork.payment.model._
import app.softnetwork.payment.spi.PaymentProviders
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

trait PaymentServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends RootPaymentEndpoints[SD]
    with CardEndpoints[SD]
    with CardPaymentEndpoints[SD]
    with BankAccountEndpoints[SD]
    with KycDocumentEndpoints[SD]
    with UboDeclarationEndpoints[SD]
    with RecurringPaymentEndpoints[SD]
    with MandateEndpoints[SD] {
  _: PaymentHandler with SessionMaterials[SD] =>

  import app.softnetwork.serialization._

  /** should be implemented by each payment provider
    */
  def hooks: List[Full[Unit, Unit, _, Unit, Unit, Any, Future]] = {
    PaymentProviders.hooksEndpoints.map { case (k, v) =>
      v.hooks(rootEndpoint.in(PaymentSettings.PaymentConfig.hooksRoute).in(k))
    }.toList
  }

  val loadPaymentAccount: ServerEndpoint[Any with AkkaStreams, Future] =
    requiredSessionEndpoint.get
      .out(jsonBody[PaymentAccountView].description("Authenticated user payment account"))
      .serverLogic { case (client, session) =>
        _ => {
          run(
            LoadPaymentAccount(
              externalUuidWithProfile(session),
              clientId = client.map(_.clientId).orElse(session.clientId)
            )
          ).map {
            case r: PaymentAccountLoaded => Right(r.paymentAccount.view)
            case other                   => Left(error(other))
          }
        }
      }
      .description("Load authenticated user payment account")

  override val endpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    cardEndpoints ++
    cardPaymentEndpoints ++
    bankAccountEndpoints ++
    kycDocumentEndpoints ++
    uboDeclarationEndpoints ++
    mandateEndpoints ++
    recurringPaymentEndpoints ++
    hooks :+ loadPaymentAccount

}

case class UploadDocument(pages: Part[Array[Byte]]) { //TODO pages may include multiple parts
  lazy val bytes: Seq[Array[Byte]] = Seq(pages.body)
}
