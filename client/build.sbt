organization := "app.softnetwork.payment"

name := "payment-client"

akkaGrpcGeneratedSources := Seq(AkkaGrpc.Client)

libraryDependencies ++= Seq(
  "app.softnetwork.account" %% "account-common" % Versions.account,
  "app.softnetwork.account" %% "account-common" % Versions.account % "protobuf",
  "app.softnetwork.api" %% "generic-server-api" % Versions.genericPersistence,
  "app.softnetwork.protobuf" %% "scalapb-extensions" % "0.1.7",
  "commons-validator" % "commons-validator" % "1.6"
)