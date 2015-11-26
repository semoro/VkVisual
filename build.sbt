name := "VkVisual"

version := "1.0"

scalaVersion := "2.11.7"

/*libraryDependencies += "org.graphstream" % "gs-core" % "1.3"
libraryDependencies += "org.graphstream" % "gs-algo" % "1.3"
libraryDependencies += "org.graphstream" % "gs-ui" % "1.3"*/
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.1"
libraryDependencies += "org.jgrapht" % "jgrapht-ext" % "0.9.1"
libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "2.3.0.5"
libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.3.0"
libraryDependencies += "com.netaporter" %% "scala-uri" % "0.4.11"

fork in run := true
javaOptions in run += "-Dsun.java2d.opengl=True"