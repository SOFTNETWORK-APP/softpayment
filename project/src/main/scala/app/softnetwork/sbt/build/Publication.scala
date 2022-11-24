package app.softnetwork.sbt.build

import java.util.Date

import sbt._

object Publication {

  def settings: Seq[Def.Setting[_]] = Seq(
    (Keys.publishTo in ThisBuild) := selectDestination((Keys.version in Keys.publish).value),
    Keys.publishMavenStyle := true,
    Keys.credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )

  val artifactoryUrl = "https://softnetwork.jfrog.io/artifactory/"

  val releasesRepository = "releases" at artifactoryUrl + "libs-release-local"

  lazy val snapshotsRepository = "snapshots" at artifactoryUrl + "libs-snapshot-local"

  private def selectDestination(version: String): Option[Resolver] =
    if(version.trim.toUpperCase.endsWith("SNAPSHOT")) {
      Some(snapshotsRepository)
    }
    else {
      Some(releasesRepository)
    }
}
