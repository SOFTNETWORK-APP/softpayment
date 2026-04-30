lazy val scala212 = "2.12.20"
lazy val scala213 = "2.13.16"
lazy val javacCompilerVersion = "17"
lazy val scalacCompilerOptions = Seq("-deprecation", "-feature")

lazy val moduleSettings = Seq(
  crossScalaVersions := Seq(scala212, scala213),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => scalacCompilerOptions :+ "-Ypartial-unification"
      case Some((2, 13)) => scalacCompilerOptions :+ s"-release:$javacCompilerVersion"
      case _             => Seq.empty
    }
  }
)

ThisBuild / organization := "app.softnetwork"

name := "payment"

ThisBuild / version := "0.9.7"

ThisBuild / scalaVersion := scala212

ThisBuild / javacOptions ++= Seq("-source", javacCompilerVersion, "-target", javacCompilerVersion, "-Xlint")

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
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.11.0",
  "org.slf4j" % "slf4j-api" % Versions.slf4j,
  "ch.qos.logback" % "logback-classic" % Versions.logback
) ++ scalatest

ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always,
  "org.scala-lang.modules" %% "scala-parser-combinators" % VersionScheme.Always
)

ThisBuild / javaOptions ++= Seq(
  "--add-opens=java.base/java.util=ALL-UNNAMED",
  "--add-opens=java.base/java.util.concurrent=ALL-UNNAMED",
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
  "--add-opens=java.base/java.math=ALL-UNNAMED",
  "--add-opens=java.base/java.io=ALL-UNNAMED",
  "--add-opens=java.base/java.net=ALL-UNNAMED",
  "--add-opens=java.base/java.nio=ALL-UNNAMED",
  "--add-opens=java.base/java.text=ALL-UNNAMED",
  "--add-opens=java.base/java.time=ALL-UNNAMED",
  "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
)

ThisBuild / Test / fork := true

ThisBuild / Test / javaOptions ++= (ThisBuild / javaOptions).value

Test / parallelExecution := false

lazy val client = project.in(file("client"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    app.softnetwork.Info.infoSettings,
    moduleSettings
  )
  .enablePlugins(BuildInfoPlugin, AkkaGrpcPlugin, JavaAppPackaging, UniversalDeployPlugin)

lazy val common = project.in(file("common"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    moduleSettings
  )
  .enablePlugins(AkkaGrpcPlugin)
  .dependsOn(
    client % "compile->compile;test->test;it->it"
  )

lazy val core = project.in(file("core"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    app.softnetwork.Info.infoSettings,
    moduleSettings
  )
  .enablePlugins(BuildInfoPlugin, AkkaGrpcPlugin)
  .dependsOn(
    common % "compile->compile;test->test;it->it"
  )

lazy val mangopay = project.in(file("mangopay"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    moduleSettings
  )
  .dependsOn(
    core % "compile->compile;test->test;it->it"
  )

lazy val stripe = project.in(file("stripe"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    moduleSettings
  )
  .dependsOn(
    core % "compile->compile;test->test;it->it"
  )

lazy val api = project.in(file("api"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    moduleSettings
  )
  .enablePlugins(DockerComposePlugin, DockerPlugin, JavaAppPackaging)
  .dependsOn(
    mangopay % "compile->compile;test->test;it->it"
  )
  .dependsOn(
    stripe % "compile->compile;test->test;it->it"
  )

lazy val testkit = project.in(file("testkit"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    moduleSettings
  )
  .dependsOn(
    mangopay % "compile->compile;test->test;it->it"
  )
  .dependsOn(
    stripe % "compile->compile;test->test;it->it"
  )

lazy val root = project.in(file("."))
  .aggregate(client, common, core, mangopay, stripe, testkit, api)
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    crossScalaVersions := Nil
  )
