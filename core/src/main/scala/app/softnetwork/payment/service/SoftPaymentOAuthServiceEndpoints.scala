package app.softnetwork.payment.service

import app.softnetwork.account.service.OAuthServiceEndpoints
import app.softnetwork.payment.handlers.SoftPaymentAccountTypeKey
import app.softnetwork.session.service.SessionMaterials
import sttp.capabilities
import sttp.capabilities.akka.AkkaStreams
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future

trait SoftPaymentOAuthServiceEndpoints
    extends OAuthServiceEndpoints
    with SoftPaymentAccountTypeKey { _: SessionMaterials =>

  override lazy val endpoints
    : List[ServerEndpoint[AkkaStreams with capabilities.WebSockets, Future]] =
    List(
      authorize
    ) ++ services.map(signin) ++ services.map(backup)

}
