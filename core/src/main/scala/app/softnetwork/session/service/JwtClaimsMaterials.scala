package app.softnetwork.session.service

import akka.actor.typed.ActorSystem
import app.softnetwork.session.handlers.JwtClaimsRefreshTokenDao
import app.softnetwork.session.model.JwtClaims
import com.softwaremill.session.{RefreshTokenStorage, SessionConfig, SessionManager}
import org.softnetwork.session.model.Session

import scala.concurrent.ExecutionContext

trait JwtClaimsMaterials {

  implicit def manager(implicit sessionConfig: SessionConfig): SessionManager[JwtClaims]

  implicit def ts: ActorSystem[_]

  implicit def ec: ExecutionContext = ts.executionContext

  implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] = JwtClaimsRefreshTokenDao(ts)

  protected def sessionType: Session.SessionType

  def headerAndForm: Boolean = false
}
