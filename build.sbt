val dottyVersion = "0.11.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2018",
    version := "0.0.1",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
