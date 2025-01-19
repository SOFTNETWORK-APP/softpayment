import com.typesafe.sbt.packager.docker._

Compile / mainClass := Some("app.softnetwork.payment.api.SoftPayEndpointsPostgresLauncher")

dockerBaseImage := "openjdk:8"

dockerEntrypoint := Seq(s"${(Docker / defaultLinuxInstallLocation).value}/bin/entrypoint.sh")

dockerExposedVolumes := Seq(
  s"${(Docker / defaultLinuxInstallLocation).value}/conf",
  s"${(Docker / defaultLinuxInstallLocation).value}/logs"
)

dockerExposedPorts := Seq(
  9000,
  5000,
  8558,
  25520
)

dockerRepository := Some("softnetwork.jfrog.io/default-docker-local")

bashScriptDefines / scriptClasspath ~= (cp => "../conf" +: cp)

organization := "app.softnetwork.payment"

name := "softpay-api"

libraryDependencies ++= Seq(
  "app.softnetwork.scheduler" %% "scheduler-api" % Versions.scheduler
)
