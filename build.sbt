val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "simple-chords-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.yaml" % "snakeyaml" % "1.32",
    // libraryDependencies += "org.virtuslab" %% "scala-yaml" % "0.0.5",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
