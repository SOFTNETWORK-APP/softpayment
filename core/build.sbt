organization := "app.softnetwork.payment"

name := "payment-core"

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-kv" % Versions.genericPersistence,
  "app.softnetwork.account" %% "account-core" % Versions.account,
  "app.softnetwork.session" %% "session-core" % Versions.genericPersistence
)
