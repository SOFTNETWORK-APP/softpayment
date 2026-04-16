organization := "app.softnetwork.payment"

name := "stripe-core"

libraryDependencies ++= Seq(
  // stripe
  "com.stripe" % "stripe-java" % "26.12.0" // TODO upgrade to v27.1.2, v28.4.0, v29.5.0, v30.2.0 and then v31.4.1
)
