package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.account.launch.AccountEndpoints
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.api.server.{ApiEndpoints, Endpoint}
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.PaymentServiceEndpoints
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import org.json4s.Formats

trait PaymentEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends AccountEndpoints[
      SoftPayAccount,
      BasicAccountProfile,
      BasicAccountSignUp,
      SD
    ] {
  _: PaymentGuardian with SchemaProvider with CsrfCheck =>

  override implicit def formats: Formats = paymentFormats

  def paymentEndpoints: ActorSystem[_] => PaymentServiceEndpoints[SD]

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system =>
      List(
        paymentEndpoints(system),
        accountEndpoints(system),
        oauthEndpoints(system)
      )
}
