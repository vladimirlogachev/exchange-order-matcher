val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "exchange-matcher",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Werror"
    ),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.1",
      "dev.zio" %% "zio-streams" % "2.1.1",
      "dev.zio" %% "zio-parser" % "0.1.9",
      "dev.zio" %% "zio-test" % "2.1.1" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1.1" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Scalafix
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
