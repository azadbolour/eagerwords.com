name := "scalautil"
organization := "com.bolour"
version := "0.9.9"
publishMavenStyle := true
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
scalaVersion := "2.12.2"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.28"
libraryDependencies += "com.typesafe" % "config" % "1.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "javax.mail" % "javax.mail-api" % "1.6.2"
libraryDependencies += "com.sun.mail" % "javax.mail" % "1.6.2"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"
libraryDependencies += "com.typesafe.slick" %% "slick" % "3.3.2"
libraryDependencies += "com.h2database" % "h2" % "1.4.185"
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.30.1"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"
libraryDependencies += "com.typesafe.play" %% "play" % "2.6.7"


