package app.softnetwork.payment.api

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.server.directives.Credentials
import app.softnetwork.api.server.DefaultComplete
import app.softnetwork.session.service.SessionService
import com.softwaremill.session.CsrfDirectives.{hmacTokenCsrfProtection, setNewCsrfToken}
import com.softwaremill.session.CsrfOptions.checkHeader
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.json4s.Formats
import org.softnetwork.session.model.Session

trait BasicServiceRoute extends Directives with DefaultComplete with Json4sSupport {

  import app.softnetwork.persistence.generateUUID
  import app.softnetwork.serialization._

  import Session._

  implicit def formats: Formats = commonFormats

  def sessionService: SessionService

  val route: Route = {
    pathPrefix("auth") {
      basic
    }
  }

  lazy val basic: Route = path("basic") {
    get {
      // check anti CSRF token
      hmacTokenCsrfProtection(checkHeader) {
        // check if a session exists
        sessionService.requiredSession { session =>
          complete(HttpResponse(StatusCodes.OK, entity = session.id))
        }
      }
    } ~ post {
      authenticateBasic("Basic Realm", BasicAuthAuthenticator) { identifier =>
        // create a new session
        val session = Session(generateUUID(identifier))
        sessionService.setSession(session) {
          // create a new anti csrf token
          setNewCsrfToken(checkHeader) {
            complete(HttpResponse(StatusCodes.OK, entity = session.id))
          }
        }
      }
    } ~ delete {
      // check anti CSRF token
      hmacTokenCsrfProtection(checkHeader) {
        // check if a session exists
        sessionService.requiredSession { _ =>
          // invalidate session
          sessionService.invalidateSession {
            complete(HttpResponse(StatusCodes.OK))
          }
        }
      }
    }
  }

  private def BasicAuthAuthenticator(credentials: Credentials): Option[String] = {
    credentials match {
      case p @ Credentials.Provided(_) => Some(p.identifier)
      case _                           => None
    }
  }

}

object BasicServiceRoute {
  def apply(_system: ActorSystem[_], _sessionService: SessionService): BasicServiceRoute = {
    new BasicServiceRoute {
      override def sessionService: SessionService = _sessionService
    }
  }
}
