package app.softnetwork.payment.service

import app.softnetwork.payment.scalatest.SoftPayAccountRouteSpec
import app.softnetwork.session.handlers.{JwtClaimsRefreshTokenDao, SessionRefreshTokenDao}
import app.softnetwork.session.model.SessionDataCompanion
import app.softnetwork.session.service.{BasicSessionMaterials, JwtSessionMaterials}
import com.softwaremill.session.RefreshTokenStorage
import org.softnetwork.session.model.{JwtClaims, Session}

package SoftPayAccount {
  package Directives {
    package OneOff {
      package Cookie {

        import app.softnetwork.payment.scalatest.SoftPayRoutesWithOneOfCookieSessionTestKit

        class SoftPayAccountRoutesWithOneOffCookieBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayRoutesWithOneOfCookieSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountRoutesWithOneOffCookieJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayRoutesWithOneOfCookieSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }

      package Header {

        import app.softnetwork.payment.scalatest.SoftPayRoutesWithOneOfHeaderSessionTestKit

        class SoftPayAccountRoutesWithOneOffHeaderBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayRoutesWithOneOfHeaderSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountRoutesWithOneOffHeaderJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayRoutesWithOneOfHeaderSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }
    }

    package Refreshable {
      package Cookie {

        import app.softnetwork.payment.scalatest.SoftPayRoutesWithRefreshableCookieSessionTestKit

        class SoftPayAccountRoutesWithRefreshableCookieBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayRoutesWithRefreshableCookieSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountRoutesWithRefreshableCookieJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayRoutesWithRefreshableCookieSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }

      package Header {

        import app.softnetwork.payment.scalatest.SoftPayRoutesWithRefreshableHeaderSessionTestKit

        class SoftPayAccountRoutesWithRefreshableHeaderBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayRoutesWithRefreshableHeaderSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountRoutesWithRefreshableHeaderJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayRoutesWithRefreshableHeaderSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }
    }
  }
  package Endpoints {
    package OneOff {
      package Cookie {

        import app.softnetwork.payment.scalatest.SoftPayEndpointsWithOneOfCookieSessionTestKit

        class SoftPayAccountEndpointsWithOneOffCookieBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayEndpointsWithOneOfCookieSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountEndpointsWithOneOffCookieJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayEndpointsWithOneOfCookieSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }

      package Header {

        import app.softnetwork.payment.scalatest.SoftPayEndpointsWithOneOfHeaderSessionTestKit

        class SoftPayAccountEndpointsWithOneOffHeaderBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayEndpointsWithOneOfHeaderSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountEndpointsWithOneOffHeaderJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayEndpointsWithOneOfHeaderSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }
    }

    package Refreshable {
      package Cookie {

        import app.softnetwork.payment.scalatest.SoftPayEndpointsWithRefreshableCookieSessionTestKit

        class SoftPayAccountEndpointsWithRefreshableCookieBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayEndpointsWithRefreshableCookieSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountEndpointsWithRefreshableCookieJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayEndpointsWithRefreshableCookieSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }

      package Header {

        import app.softnetwork.payment.scalatest.SoftPayEndpointsWithRefreshableHeaderSessionTestKit

        class SoftPayAccountEndpointsWithRefreshableHeaderBasicSessionSpec
            extends SoftPayAccountRouteSpec[Session]
            with SoftPayEndpointsWithRefreshableHeaderSessionTestKit[Session]
            with BasicSessionMaterials[Session] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[Session] =
            SessionRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[Session] = Session

        }

        class SoftPayAccountEndpointsWithRefreshableHeaderJwtSessionSpec
            extends SoftPayAccountRouteSpec[JwtClaims]
            with SoftPayEndpointsWithRefreshableHeaderSessionTestKit[JwtClaims]
            with JwtSessionMaterials[JwtClaims] {

          override implicit def refreshTokenStorage: RefreshTokenStorage[JwtClaims] =
            JwtClaimsRefreshTokenDao(ts)

          override implicit def companion: SessionDataCompanion[JwtClaims] = JwtClaims

        }
      }
    }
  }
}
