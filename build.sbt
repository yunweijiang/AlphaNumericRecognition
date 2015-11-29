name := "Recognition"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

val scalaTestVersion = "2.2.4"

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.11.6"
/*
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.0-SNAPSHOT"
*/
