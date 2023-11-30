package app.softnetwork.session.service

import app.softnetwork.session.model.JwtClaims
import com.softwaremill.session.{
  CsrfCheckMode,
  CsrfOptions,
  GetSessionTransport,
  SessionContinuity,
  SessionDirectives,
  SessionManager,
  SetSessionTransport
}
import org.softnetwork.session.model.Session

trait JwtClaimsDirectives extends SessionDirectives { _: JwtClaimsMaterials =>

  import com.softwaremill.session.SessionOptions._

  def sc(implicit manager: SessionManager[JwtClaims]): SessionContinuity[JwtClaims] =
    sessionType match {
      case Session.SessionType.OneOffCookie | Session.SessionType.OneOffHeader => oneOff
      case _                                                                   => refreshable
    }

  lazy val st: SetSessionTransport =
    sessionType match {
      case Session.SessionType.OneOffCookie | Session.SessionType.RefreshableCookie => usingCookies
      case _                                                                        => usingHeaders
    }

  lazy val gt: GetSessionTransport = st

  def checkMode(implicit manager: SessionManager[JwtClaims]): CsrfCheckMode[JwtClaims] =
    if (headerAndForm) {
      CsrfOptions.checkHeaderAndForm
    } else {
      CsrfOptions.checkHeader
    }

}
