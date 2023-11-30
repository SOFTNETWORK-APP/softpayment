package app.softnetwork.session.service

import app.softnetwork.api.server.ServiceEndpoints
import app.softnetwork.persistence.message.{Command, CommandResult}
import app.softnetwork.persistence.typed.scaladsl.Patterns

trait ServiceWithJwtClaimsEndpoints[C <: Command, R <: CommandResult]
    extends ServiceEndpoints[C, R]
    with JwtClaimsEndpoints { _: Patterns[C, R] with JwtClaimsMaterials => }
