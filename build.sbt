name := "VkVisual"

version := "1.0"

scalaVersion := "2.11.7"

//resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.3.0"
libraryDependencies += "com.netaporter" %% "scala-uri" % "0.4.11"
libraryDependencies += "org.joml" % "joml" % "1.6.6"
libraryDependencies += "com.github.scribejava" % "scribejava-apis" % "2.0.1"
libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)
/*
fork in run := true
javaOptions in run += "-Dsun.java2d.opengl=True"*/

val sbtcp = taskKey[Unit]("sbt-classpath")

sbtcp := {
  val files: Seq[File] = (fullClasspath in Compile).value.files
  val sbtClasspath: String = files.map(x => x.getAbsolutePath).mkString(":")
  println("Set SBT classpath to 'sbt-classpath' environment variable")
  System.setProperty("sbt-classpath", sbtClasspath)
}

compile <<= (compile in Compile).dependsOn(sbtcp)