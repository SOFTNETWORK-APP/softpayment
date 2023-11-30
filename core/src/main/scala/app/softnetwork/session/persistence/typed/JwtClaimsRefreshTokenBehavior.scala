package app.softnetwork.session.persistence.typed

import app.softnetwork.session.model.JwtClaims

trait JwtClaimsRefreshTokenBehavior extends RefreshTokenBehavior[JwtClaims] {
  override val persistenceId = "JwtClaims"
}

object JwtClaimsRefreshTokenBehavior extends JwtClaimsRefreshTokenBehavior
