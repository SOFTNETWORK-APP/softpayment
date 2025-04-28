organization := "app.softnetwork.payment"

name := "stripe-core"

libraryDependencies ++= Seq(
  // stripe
  "com.stripe" % "stripe-java" % "26.12.0"
)
