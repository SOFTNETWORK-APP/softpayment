package app.softnetwork.payment.launch

import akka.actor.typed.ActorSystem
import app.softnetwork.account.launch.AccountRoutes
import app.softnetwork.account.model.{
  BasicAccountProfile,
  DefaultAccountDetailsView,
  DefaultAccountView,
  DefaultProfileView
}
import app.softnetwork.api.server.ApiRoute
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.payment.serialization.paymentFormats
import app.softnetwork.persistence.schema.SchemaProvider
import app.softnetwork.session.CsrfCheck
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import org.json4s.Formats

trait SoftPayRoutes[SD <: SessionData with SessionDataDecorator[SD]]
    extends AccountRoutes[
      SoftPayAccount,
      BasicAccountProfile,
      DefaultProfileView,
      DefaultAccountDetailsView,
      DefaultAccountView[DefaultProfileView, DefaultAccountDetailsView],
      SD
    ]
    with PaymentRoutes[SD]
    with SoftPayGuardian { _: SchemaProvider with CsrfCheck =>

  override implicit def formats: Formats = paymentFormats

  override def apiRoutes: ActorSystem[_] => List[ApiRoute] =
    system =>
      List(
        paymentService(system),
        accountService(system),
        oauthService(system)
      )

}
