import app.softnetwork.sbt.build.Versions

Test / parallelExecution := false

organization := "app.softnetwork.payment"

name := "payment-testkit"

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-scheduler-testkit" % Versions.genericPersistence,
  "app.softnetwork.persistence" %% "persistence-session-testkit" % Versions.genericPersistence
)
