package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.headers.HttpChallenges
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.{AuthenticationFailedRejection, Route}
import akka.http.scaladsl.server.directives.Credentials
import app.softnetwork.account.config.AccountSettings
import app.softnetwork.api.server.GrpcService
import app.softnetwork.concurrent.Completion
import app.softnetwork.payment.handlers.SoftPayAccountDao

import scala.concurrent.Future

class PaymentGrpcService(server: PaymentServer, softPayAccountDao: SoftPayAccountDao)
    extends GrpcService
    with Completion {
  override def grpcService: ActorSystem[_] => PartialFunction[HttpRequest, Future[HttpResponse]] =
    system => PaymentServiceApiHandler.partial(server)(system)

  override def route: ActorSystem[_] => Route = system =>
    authenticateOAuth2Async(
      AccountSettings.Realm,
      {
        case _ @Credentials.Provided(token) =>
          softPayAccountDao.authenticateClient(Some(token))(system)
        case _ => Future.successful(None)
      }
    ).optional {
      case (Some(client)) =>
        handle(
          grpcService(system),
          Seq(
            AuthenticationFailedRejection(
              AuthenticationFailedRejection.CredentialsRejected,
              HttpChallenges.oAuth2(AccountSettings.Realm)
            )
          )
        )
      case _ =>
        reject(
          AuthenticationFailedRejection(
            AuthenticationFailedRejection.CredentialsRejected,
            HttpChallenges.oAuth2(AccountSettings.Realm)
          )
        )
    }

}
