name := "tshrdlu"

version := "0.1.5-SNAPSHOT"

organization := "edu.utexas"

scalaVersion := "2.10.1"

crossPaths := false

retrieveManaged := true

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.twitter4j" % "twitter4j-core" % "3.0.3",
  "org.twitter4j" % "twitter4j-stream" % "3.0.3",
  "org.scalanlp" % "nak" % "1.1.3-SNAPSHOT",
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "com.typesafe.akka" %% "akka-actor" % "2.1.2",
  "org.rogach" %% "scallop" % "0.8.1",
  "org.clapper" % "argot_2.9.1" % "0.3.8",
  "commons-codec" % "commons-codec" % "1.7",
  "org.apache.lucene" % "lucene-core" % "4.2.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.0",
  "gov.nist.math" % "jama" % "1.0.2",
  "de.bwaldvogel" % "liblinear" % "1.92"
)
