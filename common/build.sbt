organization := "app.softnetwork.payment"

name := "payment-common"

akkaGrpcGeneratedSources := Seq(AkkaGrpc.Client)

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-kv" % Versions.genericPersistence
)

Compile / unmanagedResourceDirectories += baseDirectory.value / "src/main/protobuf"
