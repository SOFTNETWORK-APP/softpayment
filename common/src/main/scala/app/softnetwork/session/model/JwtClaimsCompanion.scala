package app.softnetwork.session.model

import app.softnetwork.persistence.generateUUID
import app.softnetwork.session.config.Settings.Session._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import java.util.Base64
import scala.util.Try

trait JwtClaimsCompanion {
  val adminKey = "admin"

  val profileKey = "profile"

  val anonymousKey = "anonymous"

  val idKey: String = DefaultSessionConfig.sessionCookieConfig.name

  val clientIdKey: String = "client_id"

  val refreshable: Boolean = Continuity match {
    case "refreshable" => true
    case _             => false
  }

  def apply(): JwtClaims =
    JwtClaims.defaultInstance
      .withAdditionalClaims(Map(idKey -> generateUUID()))
      .withRefreshable(refreshable)

  def apply(s: String): JwtClaims =
    Try {
      val sCleaned = if (s.startsWith("Bearer")) s.substring(7).trim else s
      val List(_, p, _) = sCleaned.split("\\.").toList
      val decodedValue = Try {
        parse(new String(Base64.getUrlDecoder.decode(p), "utf-8"))
      }
      for (jv <- decodedValue) yield {
        val iss = jv \\ "iss" match {
          case JString(value) => Some(value)
          case _              => None
        }
        val sub = jv \\ "sub" match {
          case JString(value) => Some(value)
          case _              => None
        }
        val aud = jv \\ "aud" match {
          case JString(value) => Some(value)
          case _              => None
        }
        val exp = jv \\ "exp" match {
          case JInt(value) => Some(value.toLong)
          case _           => None
        }
        val nbf = jv \\ "nbf" match {
          case JInt(value) => Some(value.toLong)
          case _           => None
        }
        val iat = jv \\ "iat" match {
          case JInt(value) => Some(value.toLong)
          case _           => None
        }
        val jti = jv \\ "jti" match {
          case JString(value) => Some(value)
          case _              => None
        }
        JwtClaims(
          Map.empty,
          refreshable,
          iss,
          sub,
          aud,
          exp,
          nbf,
          iat,
          jti
        ).withId(sub.getOrElse(generateUUID()))
      }
    }.flatten.toOption.getOrElse(JwtClaims())
}
