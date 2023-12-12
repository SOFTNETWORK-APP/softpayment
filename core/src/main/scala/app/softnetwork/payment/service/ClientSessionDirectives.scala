package app.softnetwork.payment.service

import akka.http.scaladsl.server.Directives.authenticateOAuth2Async
import akka.http.scaladsl.server.{Directive1, Route}
import akka.http.scaladsl.server.directives.Credentials
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.payment.annotation.InternalApi
import app.softnetwork.payment.model.SoftPayAccount
import app.softnetwork.session.model.{SessionData, SessionDataDecorator}
import app.softnetwork.session.service.{SessionMaterials, SessionService}

import scala.concurrent.Future

trait ClientSessionDirectives[SD <: SessionData with SessionDataDecorator[SD]]
    extends SessionService[SD]
    with ClientSession[SD] {
  _: SessionMaterials[SD] =>

  @InternalApi
  private[payment] def clientDirective: Directive1[Option[SoftPayAccount.SoftPayClient]] =
    authenticateOAuth2Async(AccountSettings.Realm, oauthClient).optional

  @InternalApi
  private[payment] def requiredClientSession(
    body: (Option[SoftPayAccount.SoftPayClient], SD) => Route
  ): Route =
    clientDirective { client =>
      requiredSession(sc(clientSessionManager(client)), gt) { session =>
        body(client, session)
      }
    }

  @InternalApi
  private[payment] def optionalClientSession(
    body: (Option[SoftPayAccount.SoftPayClient], Option[SD]) => Route
  ): Route =
    clientDirective { client =>
      optionalSession(sc(clientSessionManager(client)), gt) { session =>
        body(client, session)
      }
    }

  @InternalApi
  private[payment] def oauthClient: Credentials => Future[Option[SoftPayAccount.SoftPayClient]] = {
    case _ @Credentials.Provided(token) => softPayAccountDao.authenticateClient(Some(token))
    case _                              => Future.successful(None)
  }

}
