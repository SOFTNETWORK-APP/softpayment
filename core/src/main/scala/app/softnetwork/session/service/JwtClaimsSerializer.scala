package app.softnetwork.session.service

import app.softnetwork.session.model.JwtClaims
import com.softwaremill.session.SessionSerializer
import org.json4s.jackson.JsonMethods.asJValue
import org.json4s.{DefaultFormats, DefaultWriters, Formats, JValue}

import scala.util.Try

case object JwtClaimsSerializer extends SessionSerializer[JwtClaims, JValue] {

  implicit val formats: Formats = DefaultFormats

  import DefaultWriters._

  override def serialize(t: JwtClaims): JValue = asJValue(t.additionalClaims)

  override def deserialize(r: JValue): Try[JwtClaims] = Try {
    JwtClaims(additionalClaims = r.extract[Map[String, String]])
  }
}
