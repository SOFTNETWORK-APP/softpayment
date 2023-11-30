package app.softnetwork.payment.service

import akka.http.scaladsl.server.Directives.authenticateOAuth2Async
import akka.http.scaladsl.server.{Directive1, Route}
import akka.http.scaladsl.server.directives.Credentials
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.model.SoftPaymentAccount
import app.softnetwork.session.service.{SessionMaterials, SessionService}
import org.softnetwork.session.model.Session

import scala.concurrent.Future

trait ClientSessionDirectives extends SessionService with ClientSession { _: SessionMaterials =>

  @InternalApi
  private[payment] def clientDirective: Directive1[Option[SoftPaymentAccount.Client]] =
    authenticateOAuth2Async(AccountSettings.Realm, oauthClient).optional

  @InternalApi
  private[payment] def requiredClientSession(
    body: (Option[SoftPaymentAccount.Client], Session) => Route
  ): Route =
    clientDirective { client =>
      requiredSession(sc(clientSessionManager(client)), gt) { session =>
        body(client, session)
      }
    }

  @InternalApi
  private[payment] def optionalClientSession(
    body: (Option[SoftPaymentAccount.Client], Option[Session]) => Route
  ): Route =
    clientDirective { client =>
      optionalSession(sc(clientSessionManager(client)), gt) { session =>
        body(client, session)
      }
    }

  @InternalApi
  private[payment] def oauthClient: Credentials => Future[Option[SoftPaymentAccount.Client]] = {
    case _ @Credentials.Provided(token) => toClient(Some(token))
    case _                              => Future.successful(None)
  }

}
