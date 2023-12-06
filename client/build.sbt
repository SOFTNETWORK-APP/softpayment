organization := "app.softnetwork.payment"

name := "payment-client"

akkaGrpcGeneratedSources := Seq(AkkaGrpc.Client)

val jacksonExclusions = Seq(
  ExclusionRule(organization = "com.fasterxml.jackson.core"),
  ExclusionRule(organization = "com.fasterxml.jackson.databind"),
  ExclusionRule(organization = "com.fasterxml.jackson.jaxrs"),
  ExclusionRule(organization = "com.fasterxml.jackson.module"),
  ExclusionRule(organization = "com.fasterxml.jackson.dataformat", "jackson-dataformat-yaml")
)

libraryDependencies ++= Seq(
  "app.softnetwork.account" %% "account-common" % Versions.account,
  "app.softnetwork.account" %% "account-common" % Versions.account % "protobuf",
  "app.softnetwork.api" %% "generic-server-api" % Versions.genericPersistence,
  "app.softnetwork.protobuf" %% "scalapb-extensions" % "0.1.7",
  "commons-validator" % "commons-validator" % "1.6",
  "com.github.scopt" %% "scopt" % Versions.scopt,
  "org.scalatra.scalate" %% "scalate-core" % Versions.scalate exclude ("org.scala-lang.modules", "scala-xml_2.12") exclude ("org.scala-lang.modules", "scala-parser-combinators_2.12"),
  "com.hubspot.jinjava" % "jinjava" % Versions.jinja excludeAll (jacksonExclusions *) exclude ("com.google.guava", "guava") exclude ("org.apache.commons", "commons-lang3")
)
