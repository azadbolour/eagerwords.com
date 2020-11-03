name := "auth"
organization := "com.bolour"
version := "0.9.9"
publishMavenStyle := true
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
scalaVersion := "2.12.2"

val akkaVersion = "2.5.6"
dependencyOverrides ++= Seq( // Seq for SBT 1.0.x
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.google.guava" % "guava" % "22.0",
)

libraryDependencies += "javax.inject" % "javax.inject" % "1"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % "test"

val circeVersion = "0.12.3"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
