package app.softnetwork.session.service

import app.softnetwork.session.model.JwtClaims
import app.softnetwork.session.{
  TapirCsrfCheckMode,
  TapirCsrfOptions,
  TapirEndpoints,
  TapirSessionContinuity
}
import com.softwaremill.session.{GetSessionTransport, SessionManager, SetSessionTransport}
import org.softnetwork.session.model.Session

trait JwtClaimsEndpoints extends TapirEndpoints {
  _: JwtClaimsMaterials =>

  import app.softnetwork.session.TapirSessionOptions._

  def sc(implicit manager: SessionManager[JwtClaims]): TapirSessionContinuity[JwtClaims] =
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

  def checkMode(implicit manager: SessionManager[JwtClaims]): TapirCsrfCheckMode[JwtClaims] =
    if (headerAndForm) {
      TapirCsrfOptions.checkHeaderAndForm
    } else {
      TapirCsrfOptions.checkHeader
    }

}
