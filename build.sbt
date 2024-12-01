val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "squarereflect",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-Xfatal-warnings",
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      // "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      // "org.jline" % "jline3" % "3.14.0",
      // "org.lwjgl" % "lwjgl" % "3.3.4",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
