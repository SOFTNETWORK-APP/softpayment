import app.softnetwork.sbt.build._

organization := "app.softnetwork.payment"

name := "payment-core"

libraryDependencies ++= Seq(
  "app.softnetwork.persistence" %% "persistence-kv" % Versions.genericPersistence,
  "app.softnetwork.persistence" %% "persistence-session" % Versions.genericPersistence
)
