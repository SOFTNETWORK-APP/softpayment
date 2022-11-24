package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.server.directives.Credentials
import app.softnetwork.api.server.DefaultComplete
import app.softnetwork.session.service.SessionService
import com.softwaremill.session.CsrfDirectives.{randomTokenCsrfProtection, setNewCsrfToken}
import com.softwaremill.session.CsrfOptions.checkHeader
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.Formats
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait BasicServiceRoute extends SessionService with Directives with DefaultComplete with Json4sSupport {

  import app.softnetwork.persistence.generateUUID
  import app.softnetwork.serialization._

  import Session._

  implicit def formats: Formats = commonFormats

  implicit lazy val ec: ExecutionContext = system.executionContext

  val route: Route = {
    pathPrefix("auth") {
      basic
    }
  }

  lazy val basic: Route = path("basic"){
    get {
      // check anti CSRF token
      randomTokenCsrfProtection(checkHeader) {
        // check if a session exists
        _requiredSession(ec) { session =>
          complete(HttpResponse(StatusCodes.OK, entity = session.id))
        }
      }
    } ~ post {
      authenticateBasic("Basic Realm", BasicAuthAuthenticator) { identifier =>
        // create a new session
        val session = Session(generateUUID(identifier))
        sessionToDirective(session)(ec) {
          // create a new anti csrf token
          setNewCsrfToken(checkHeader) {
            complete(HttpResponse(StatusCodes.OK, entity = session.id))
          }
        }
      }
    } ~ delete {
      // check anti CSRF token
      randomTokenCsrfProtection(checkHeader) {
        // check if a session exists
        _requiredSession(ec) { _ =>
          // invalidate session
          _invalidateSession(ec) {
            complete(HttpResponse(StatusCodes.OK))
          }
        }
      }
    }
  }

  private def BasicAuthAuthenticator(credentials: Credentials): Option[String] = {
    credentials match {
      case p@Credentials.Provided(_) => Some(p.identifier)
      case _ => None
    }
  }

}

object BasicServiceRoute {
  def apply(_system: ActorSystem[_]): BasicServiceRoute = {
    new BasicServiceRoute {
      override implicit def system: ActorSystem[_] = _system
    }
  }
}