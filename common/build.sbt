organization := "app.softnetwork.payment"

name := "payment-common"

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-kv" % Versions.genericPersistence,
  // scheduler
  "app.softnetwork.scheduler" %% "scheduler-common" % Versions.scheduler,
  "app.softnetwork.scheduler" %% "scheduler-common" % Versions.scheduler % "protobuf",
  // account
  "app.softnetwork.account" %% "account-common" % Versions.account,
  "app.softnetwork.account" %% "account-common" % Versions.account % "protobuf",
  "app.softnetwork.api" %% "generic-server-api" % Versions.genericPersistence,
  "app.softnetwork.protobuf" %% "scalapb-extensions" % "0.1.7",
  "commons-validator" % "commons-validator" % "1.6"
)

Compile / unmanagedResourceDirectories += baseDirectory.value / "src/main/protobuf"
