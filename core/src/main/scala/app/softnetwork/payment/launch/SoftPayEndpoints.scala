package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.account.launch.AccountEndpoints
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.api.server.Endpoint
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import org.json4s.Formats

trait SoftPayEndpoints[SD <: SessionData with SessionDataDecorator[SD]]
    extends AccountEndpoints[
      SoftPayAccount,
      BasicAccountProfile,
      BasicAccountSignUp,
      SD
    ]
    with PaymentEndpoints[SD]
    with SoftPayGuardian {
  _: SchemaProvider with CsrfCheck =>

  override implicit def formats: Formats = paymentFormats

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system =>
      List(
        paymentEndpoints(system),
        accountEndpoints(system),
        oauthEndpoints(system)
      )
}
