ThisBuild / organization := "app.softnetwork"

name := "payment"

ThisBuild / version := "0.8.2"

ThisBuild / scalaVersion := "2.12.18"

ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-target:jvm-1.8", "-Ypartial-unification")

ThisBuild / javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

ThisBuild / resolvers ++= Seq(
  "Softnetwork Server" at "https://softnetwork.jfrog.io/artifactory/releases/",
  "Softnetwork snapshots" at "https://softnetwork.jfrog.io/artifactory/snapshots/",
  "Maven Central Server" at "https://repo1.maven.org/maven2",
  "Typesafe Server" at "https://repo.typesafe.com/typesafe/releases"
)

ThisBuild / libraryDependencySchemes ++= Seq(
  "app.softnetwork.notification" %% "notification-common" % VersionScheme.Always,
  "app.softnetwork.notification" %% "notification-core" % VersionScheme.Always,
  "app.softnetwork.notification" %% "notification-testkit" % VersionScheme.Always
)

ThisBuild / versionScheme := Some("early-semver")

val scalatest = Seq(
  "org.scalatest" %% "scalatest" % Versions.scalatest  % Test
)

ThisBuild / libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
) ++ scalatest

Test / parallelExecution := false

lazy val client = project.in(file("client"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings, app.softnetwork.Info.infoSettings)
  .enablePlugins(BuildInfoPlugin, AkkaGrpcPlugin, JavaAppPackaging, UniversalDeployPlugin)

lazy val common = project.in(file("common"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .enablePlugins(AkkaGrpcPlugin)
  .dependsOn(
    client % "compile->compile;test->test;it->it"
  )

lazy val core = project.in(file("core"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings, app.softnetwork.Info.infoSettings)
  .enablePlugins(BuildInfoPlugin, AkkaGrpcPlugin)
  .dependsOn(
    common % "compile->compile;test->test;it->it"
  )

lazy val mangopay = project.in(file("mangopay"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .dependsOn(
    core % "compile->compile;test->test;it->it"
  )

lazy val stripe = project.in(file("stripe"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .dependsOn(
    core % "compile->compile;test->test;it->it"
  )

lazy val api = project.in(file("api"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .enablePlugins(DockerComposePlugin, DockerPlugin, JavaAppPackaging)
  .dependsOn(
    mangopay % "compile->compile;test->test;it->it"
  )
  .dependsOn(
    stripe % "compile->compile;test->test;it->it"
  )

lazy val testkit = project.in(file("testkit"))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .dependsOn(
    mangopay % "compile->compile;test->test;it->it"
  )
  .dependsOn(
    stripe % "compile->compile;test->test;it->it"
  )

lazy val root = project.in(file("."))
  .aggregate(client, common, core, mangopay, stripe, testkit, api)
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
