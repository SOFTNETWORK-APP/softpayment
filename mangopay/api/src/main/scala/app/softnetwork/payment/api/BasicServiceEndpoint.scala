package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import app.softnetwork.api.server.ApiEndpoint
import app.softnetwork.session.service.SessionEndpoints
import org.softnetwork.session.model.Session
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir._
import sttp.tapir.model.UsernamePassword
import sttp.tapir.server.ServerEndpoint

import scala.concurrent.Future
import scala.language.implicitConversions

trait BasicServiceEndpoint extends ApiEndpoint {

  def sessionEndpoints: SessionEndpoints

  implicit def usernamePassword2Session(credentials: UsernamePassword): Option[Session] = {
    Some(Session(credentials.username))
  }

  val basic: Endpoint[Unit, UsernamePassword, Unit, Unit, Any] =
    endpoint
      .in(auth.basic[UsernamePassword](WWWAuthenticateChallenge.basic("Basic Realm")))

  val createSessionEndpoint: ServerEndpoint[Any, Future] = {
    sessionEndpoints.transport
      .setSession(basic)
      .post
      .in("auth" / "basic")
      .out(sessionEndpoints.csrfCookie)
      .serverLogic(_ =>
        _ => Future.successful(Right(Some(sessionEndpoints.setNewCsrfToken().valueWithMeta)))
      )
  }

  val invalidateSessionEndpoint: ServerEndpoint[Any, Future] =
    sessionEndpoints.continuity
      .invalidateSession(
        sessionEndpoints.antiCsrfWithRequiredSession
      )
      .delete
      .in("auth" / "basic")
      .serverLogic(_ => _ => Future.successful(Right()))

  override def endpoints: List[ServerEndpoint[Any, Future]] =
    List(createSessionEndpoint, invalidateSessionEndpoint)

}

object BasicServiceEndpoint {
  def apply(_sessionEndpoints: SessionEndpoints): BasicServiceEndpoint =
    new BasicServiceEndpoint {
      override def sessionEndpoints: SessionEndpoints = _sessionEndpoints
    }
}
