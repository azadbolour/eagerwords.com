name := "scalautil"
organization := "com.bolour"
version := "0.9.5"
publishMavenStyle := true
publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
scalaVersion := "2.12.2"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.28"
libraryDependencies += "com.typesafe" % "config" % "1.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "javax.mail" % "javax.mail-api" % "1.6.2"
libraryDependencies += "com.sun.mail" % "javax.mail" % "1.6.2"

