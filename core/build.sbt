organization := "app.softnetwork.payment"

name := "payment-core"

akkaGrpcGeneratedSources := Seq(AkkaGrpc.Server)

Compile / PB.protoSources := Seq(sourceDirectory.value / ".." / ".." / "client/src/main/protobuf/api")

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-kv" % Versions.genericPersistence,
  "app.softnetwork.account" %% "account-core" % Versions.account,
  "app.softnetwork.session" %% "session-core" % Versions.genericPersistence
)
