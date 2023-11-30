package app.softnetwork.session.model

trait JwtClaimsDecorator { self: JwtClaims =>

  import JwtClaims._

  private var dirty: Boolean = false

  def isDirty: Boolean = dirty

  def get(key: String): Option[String] = additionalClaims.get(key)

  def isEmpty: Boolean = additionalClaims.isEmpty

  def contains(key: String): Boolean = additionalClaims.contains(key)

  def -(key: String): JwtClaims =
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims - key)
    }

  def +(claim: (String, String)): JwtClaims =
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims + claim)
    }

  def ++(claims: Seq[(String, String)]): JwtClaims =
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims ++ claims)
    }

  def apply(key: String): String = additionalClaims(key)

  lazy val id: String = sub.getOrElse(additionalClaims(idKey))

  def withId(id: String): JwtClaims = {
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims + (idKey -> id)).withSub(id)
    }
  }

  lazy val clientId: String = iss.getOrElse(additionalClaims(clientIdKey))

  def withClientId(clientId: String): JwtClaims = {
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims + (clientIdKey -> clientId)).withIss(clientId)
    }
  }

  lazy val admin: Boolean = get(adminKey).exists(_.toBoolean)

  def withAdmin(admin: Boolean): JwtClaims = {
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims + (adminKey -> admin.toString))
    }
  }

  lazy val anonymous: Boolean = get(anonymousKey).exists(_.toBoolean)

  def withAnonymous(anonymous: Boolean): JwtClaims = {
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims + (anonymousKey -> anonymous.toString))
    }
  }
  lazy val profile: Option[String] = get(profileKey)

  def withProfile(profile: Option[String]): JwtClaims = {
    synchronized {
      dirty = true
      withAdditionalClaims(additionalClaims + (profileKey -> profile.getOrElse("")))
    }
  }
}
