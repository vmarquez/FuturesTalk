name := "futurestalk"

version := ".1-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers ++= Seq("Sonatype Nexus releases" at "https://oss.sonatype.org/content/repositories/releases",
                 "Sonatype Nexus snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies += "org.scalaz" % "scalaz-effect_2.11" % "7.1.0"

libraryDependencies += "org.scalaz" % "scalaz-concurrent_2.11" % "7.1.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

initialCommands in console := "import scalaz._;import Scalaz._;import scala.concurrent.Future; import scala.reflect.runtime.universe.reify; import scala.concurrent.ExecutionContext.Implicits.global;"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds")
