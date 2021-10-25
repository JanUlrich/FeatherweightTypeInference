enablePlugins(ScalaJSPlugin)

name := "FJ-Typeinference"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
