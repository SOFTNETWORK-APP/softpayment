Test / parallelExecution := false

organization := "app.softnetwork.payment"

name := "payment-testkit"

libraryDependencies ++= Seq(
  "app.softnetwork.scheduler" %% "scheduler-testkit" % Versions.scheduler,
  "app.softnetwork.api" %% "generic-server-api-testkit" % Versions.genericPersistence,
  "app.softnetwork.session" %% "session-testkit" % Versions.genericPersistence,
  "app.softnetwork.persistence" %% "persistence-core-testkit" % Versions.genericPersistence,
  "app.softnetwork.account" %% "account-testkit" % Versions.account,
  "org.scalatest" %% "scalatest" % Versions.scalatest
)
