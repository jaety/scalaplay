name := "nodes"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided" 

mainClass in Compile := Some("jt.scalaplay.Nodes")
