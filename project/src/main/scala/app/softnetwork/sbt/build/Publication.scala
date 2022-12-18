package app.softnetwork.sbt.build

import sbt._

object Publication {

  def settings: Seq[Def.Setting[_]] = Seq(
    ThisBuild / Keys.publishTo := selectDestination((Keys.publish / Keys.version).value),
    ThisBuild / Keys.publishMavenStyle := true,
    ThisBuild / Keys.credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )

  val artifactoryUrl = "https://softnetwork.jfrog.io/artifactory/"

  val releasesRepository = "releases" at artifactoryUrl + "libs-release-local"

  lazy val snapshotsRepository = "snapshots" at artifactoryUrl + "libs-snapshot-local"

  private def selectDestination(version: String): Option[Resolver] =
    if (version.trim.toUpperCase.endsWith("SNAPSHOT")) {
      Some(snapshotsRepository)
    } else {
      Some(releasesRepository)
    }
}
