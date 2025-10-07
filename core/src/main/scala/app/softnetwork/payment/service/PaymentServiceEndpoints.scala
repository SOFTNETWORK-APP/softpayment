package app.softnetwork.payment.service

import app.softnetwork.payment.config.PaymentSettings
import app.softnetwork.payment.handlers.PaymentHandler
import app.softnetwork.payment.spi.PaymentProviders
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.SessionMaterials
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.model.Part
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ServerEndpoint.Full

import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}

trait PaymentServiceEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends RootPaymentEndpoints[SD]
    with PaymentMethodEndpoints[SD]
    with CheckoutEndpoints[SD]
    with PaymentAccountEndpoints[SD]
    with BankAccountEndpoints[SD]
    with KycDocumentEndpoints[SD]
    with UboDeclarationEndpoints[SD]
    with RecurringPaymentEndpoints[SD]
    with MandateEndpoints[SD] {
  _: PaymentHandler with SessionMaterials[SD] =>

  /** should be implemented by each payment provider
    */
  def hooks: List[Full[Unit, Unit, _, Unit, Unit, Any, Future]] = {
    PaymentProviders.hooksEndpoints.map { case (k, v) =>
      v.hooks(rootEndpoint.in(PaymentSettings.PaymentConfig.hooksRoute).in(k))
    }.toList
  }

  override val endpoints: List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    cardEndpoints ++
    cardPaymentEndpoints ++
    paymentAccountEndpoints ++
    bankAccountEndpoints ++
    kycDocumentEndpoints ++
    uboDeclarationEndpoints ++
    mandateEndpoints ++
    recurringPaymentEndpoints ++
    hooks

}

case class UploadDocument(pages: Part[Array[Byte]]) { //TODO pages may include multiple parts
  lazy val bytes: Seq[Array[Byte]] = Seq(pages.body)
}
