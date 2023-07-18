package app.softnetwork.payment.api

import app.softnetwork.api.server.ApiEndpoint
import app.softnetwork.session.service.SessionEndpoints
import com.softwaremill.session.{
  GetSessionTransport,
  SetSessionTransport,
  TapirCsrfCheckMode,
  TapirEndpoints,
  TapirSessionContinuity
}
import org.softnetwork.session.model.Session
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir._
import sttp.tapir.model.UsernamePassword
import sttp.tapir.server.{PartialServerEndpointWithSecurityOutput, ServerEndpoint}

import scala.concurrent.Future
import scala.language.implicitConversions

trait BasicServiceEndpoint extends ApiEndpoint with TapirEndpoints {

  def sessionEndpoints: SessionEndpoints

  def sc: TapirSessionContinuity[Session] = sessionEndpoints.sc

  def st: SetSessionTransport = sessionEndpoints.st

  def gt: GetSessionTransport = sessionEndpoints.gt

  def checkMode: TapirCsrfCheckMode[Session] = sessionEndpoints.checkMode

  implicit def usernamePassword2Session(credentials: UsernamePassword): Option[Session] = {
    Some(Session(credentials.username))
  }

  val emptySecurityEndpoint
    : PartialServerEndpointWithSecurityOutput[Unit, Unit, Unit, Unit, Unit, Unit, Any, Future] =
    endpoint.serverSecurityLogicSuccessWithOutput(_ => Future.successful(((), ())))

  val createSessionEndpoint: ServerEndpoint[Any, Future] = {
    hmacTokenCsrfProtection(checkMode) {
      setSessionWithAuth(sc, st)(
        auth.basic[UsernamePassword](WWWAuthenticateChallenge.basic("Basic Realm"))
      )
    }.post
      .in("auth" / "basic")
      .serverLogicSuccess(_ => _ => Future.successful(()))
  }

  val invalidateSessionEndpoint: ServerEndpoint[Any, Future] =
    sessionEndpoints
      .invalidateSession(sc, st) {
        emptySecurityEndpoint
      }
      .delete
      .in("auth" / "basic")
      .serverLogicSuccess(_ => _ => Future.successful(()))

  override def endpoints: List[ServerEndpoint[Any, Future]] =
    List(createSessionEndpoint, invalidateSessionEndpoint)

}

object BasicServiceEndpoint {
  def apply(_sessionEndpoints: SessionEndpoints): BasicServiceEndpoint =
    new BasicServiceEndpoint {
      override def sessionEndpoints: SessionEndpoints = _sessionEndpoints
    }
}
