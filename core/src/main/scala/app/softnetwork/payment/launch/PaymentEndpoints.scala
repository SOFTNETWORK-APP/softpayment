package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.account.launch.AccountEndpoints
import app.softnetwork.account.message.BasicAccountSignUp
import app.softnetwork.account.model.BasicAccountProfile
import app.softnetwork.api.server.{ApiEndpoints, Endpoint}
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentEndpoints
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import org.json4s.Formats

trait PaymentEndpoints
    extends AccountEndpoints[SoftPaymentAccount, BasicAccountProfile, BasicAccountSignUp] {
  _: PaymentGuardian with SchemaProvider with CsrfCheck =>

  override implicit def formats: Formats = paymentFormats

  def paymentEndpoints: ActorSystem[_] => GenericPaymentEndpoints

  override def endpoints: ActorSystem[_] => List[Endpoint] =
    system =>
      List(
        paymentEndpoints(system),
        accountEndpoints(system),
        oauthEndpoints(system)
      )
}
