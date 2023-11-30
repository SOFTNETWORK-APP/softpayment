package app.softnetwork.session.handlers

import akka.actor.typed.ActorSystem
import akka.cluster.sharding.typed.scaladsl.EntityTypeKey
import app.softnetwork.persistence.typed.CommandTypeKey
import app.softnetwork.session.message.RefreshTokenCommand
import app.softnetwork.session.model.JwtClaims
import app.softnetwork.session.persistence.typed.JwtClaimsRefreshTokenBehavior
import org.slf4j.{Logger, LoggerFactory}

import scala.reflect.ClassTag

trait JwtClaimsRefreshTokenTypeKey extends CommandTypeKey[RefreshTokenCommand] {
  override def TypeKey(implicit
    tTag: ClassTag[RefreshTokenCommand]
  ): EntityTypeKey[RefreshTokenCommand] =
    JwtClaimsRefreshTokenBehavior.TypeKey
}
trait JwtClaimsRefreshTokenHandler
    extends RefreshTokenHandler[JwtClaims]
    with JwtClaimsRefreshTokenTypeKey

trait JwtClaimsRefreshTokenDao extends RefreshTokenDao[JwtClaims] with JwtClaimsRefreshTokenHandler

object JwtClaimsRefreshTokenDao {
  def apply(asystem: ActorSystem[_]): JwtClaimsRefreshTokenDao = {
    new JwtClaimsRefreshTokenDao() {
      override implicit val system: ActorSystem[_] = asystem

      override lazy val log: Logger = LoggerFactory getLogger getClass.getName
    }
  }
}
