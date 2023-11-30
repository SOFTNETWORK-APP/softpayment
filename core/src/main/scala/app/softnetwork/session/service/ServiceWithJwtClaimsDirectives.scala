package app.softnetwork.session.service

import app.softnetwork.api.server.ApiRoute
import app.softnetwork.persistence.message.{Command, CommandResult}
import app.softnetwork.persistence.service.Service
import app.softnetwork.persistence.typed.scaladsl.Patterns

trait ServiceWithJwtClaimsDirectives[C <: Command, R <: CommandResult]
    extends Service[C, R]
    with ApiRoute
    with JwtClaimsDirectives { _: Patterns[C, R] with JwtClaimsMaterials => }
