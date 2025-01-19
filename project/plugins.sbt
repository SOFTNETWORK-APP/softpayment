logLevel := Level.Warn

libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Softnetwork releases" at "https://softnetwork.jfrog.io/artifactory/releases/",
  "Softnetwork snapshots" at "https://softnetwork.jfrog.io/artifactory/snapshots/"
)

addSbtPlugin("app.softnetwork.sbt-softnetwork" % "sbt-softnetwork-git" % "0.1.7")

addSbtPlugin("app.softnetwork.sbt-softnetwork" % "sbt-softnetwork-info" % "0.1.7")

addSbtPlugin("app.softnetwork.sbt-softnetwork" % "sbt-softnetwork-publish" % "0.1.7")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.10")

addSbtPlugin("com.tapad" % "sbt-docker-compose" % "1.0.34")

addDependencyTreePlugin

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.8")
