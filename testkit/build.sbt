import app.softnetwork.sbt.build.Versions

Test / parallelExecution := false

organization := "app.softnetwork.payment"

name := "payment-testkit"

libraryDependencies ++= Seq(
  "app.softnetwork.scheduler" %% "scheduler-testkit" % Versions.scheduler,
  "app.softnetwork.api" %% "generic-server-api-testkit" % Versions.server,
  "app.softnetwork.session" %% "session-testkit" % Versions.session
)
