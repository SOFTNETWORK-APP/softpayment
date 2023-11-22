package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.account.launch.AccountRoutes
import app.softnetwork.account.model.{
  BasicAccountProfile,
  DefaultAccountDetailsView,
  DefaultAccountView,
  DefaultProfileView
}
import app.softnetwork.api.server.{ApiRoute, ApiRoutes}
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.payment.service.GenericPaymentService
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import org.json4s.Formats

trait PaymentRoutes
    extends AccountRoutes[
      SoftPaymentAccount,
      BasicAccountProfile,
      DefaultProfileView,
      DefaultAccountDetailsView,
      DefaultAccountView[DefaultProfileView, DefaultAccountDetailsView]
    ] { _: PaymentGuardian with SchemaProvider with CsrfCheck =>

  override implicit def formats: Formats = paymentFormats

  def paymentService: ActorSystem[_] => GenericPaymentService

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system =>
      List(
        paymentService(system),
        accountService(system),
        oauthService(system)
      )

}
