package app.softnetwork.session.service

import akka.actor.typed.ActorSystem
import app.softnetwork.session.handlers.ApiKeyDao
import app.softnetwork.session.model.JwtClaims
import com.softwaremill.session.{SessionConfig, SessionEncoder, SessionManager}

import scala.language.reflectiveCalls

trait JwtClaimsManager extends JwtClaimsMaterials { self: { def apiKeyDao: ApiKeyDao } =>

  override def manager(implicit sessionConfig: SessionConfig): SessionManager[JwtClaims] = {
    implicit val encoder: SessionEncoder[JwtClaims] = new JwtClaimsEncoder {
      override implicit def system: ActorSystem[_] = self.ts
      override def apiKeyDao: ApiKeyDao = self.apiKeyDao
    }
    new SessionManager[JwtClaims](sessionConfig)
  }

}
