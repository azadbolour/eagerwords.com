name := "eagerwords-server"
organization := "com.bolour.eagerwords.scala"
version := "0.9.3"
publishMavenStyle := true
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

lazy val scalautil = project

lazy val kernel = (project in file("kernel"))
    .enablePlugins(PlayScala)
    .dependsOn(scalautil % "test->test;compile->compile")

lazy val plane = (project in file("plane"))
    .dependsOn(scalautil % "test->test;compile->compile")

lazy val `scala-server` = (project in file("."))
  .enablePlugins(PlayScala)
    .dependsOn(scalautil % "test->test;compile->compile")
    .dependsOn(kernel % "test->test;compile->compile")
    .dependsOn(plane % "test->test;compile->compile")

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.10"

val akkaVersion = "2.5.6"
dependencyOverrides ++= Seq( // Seq for SBT 1.0.x
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.google.guava" % "guava" % "22.0",
  "org.slf4j" % "slf4j-api" % "1.7.28"
)

libraryDependencies ++= Seq( jdbc , ehcache , ws , guice )
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % "test"
libraryDependencies += "com.typesafe.slick" %% "slick" % "3.3.2"
libraryDependencies += "com.h2database" % "h2" % "1.4.185"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.30.1"
libraryDependencies += "org.postgresql" % "postgresql" % "42.2.14"

val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

unmanagedResourceDirectories in Test +=  baseDirectory.value / "target/web/public/test"
// unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

