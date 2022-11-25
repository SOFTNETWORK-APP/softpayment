import app.softnetwork.sbt.build._

organization := "app.softnetwork.payment"

name := "payment-common"

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-kv" % Versions.genericPersistence,
  "app.softnetwork.persistence" %% "persistence-scheduler" % Versions.genericPersistence,
  "app.softnetwork.persistence" %% "persistence-scheduler" % Versions.genericPersistence % "protobuf",
  "app.softnetwork.api" %% "generic-server-api" % Versions.genericPersistence,
  "app.softnetwork.protobuf" %% "scalapb-extensions" % "0.1.5",
  "commons-validator" % "commons-validator" % "1.6"
)

Compile / unmanagedResourceDirectories += baseDirectory.value / "src/main/protobuf"
