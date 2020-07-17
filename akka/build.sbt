name := "chat-app"

version := "1.0"

scalaVersion := "2.13.1"
// semanticdbEnabled := true // enable SemanticDB
// semanticdbVersion := scalafixSemanticdb.revision // use Scalafix compatible version

// addCompilerPlugin(scalafixSemanticdb) // enable SemanticDB
scalacOptions ++= List(
  "-Yrangepos",          // required by SemanticDB compiler plugin
  // "-Ywarn-unused-import" // required by `RemoveUnused` rule
)

lazy val akkaVersion = "2.6.7"

// addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.3")
// addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.19")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)
