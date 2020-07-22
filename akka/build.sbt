enablePlugins(JavaAppPackaging)

name := "chat-app"
version := "1.0"
scalaVersion := "2.13.1"
organization := "chatapp"
val akkaVersion = "2.6.8"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
)

mainClass in Compile := Some("chatapp.Main")
discoveredMainClasses in Compile := Seq()
