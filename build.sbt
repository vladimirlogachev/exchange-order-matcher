val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "exchange-order-matcher",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    run / fork   := true, // Makes exit codes work as expected
    libraryDependencies ++= Seq(
      "dev.zio"    %% "zio"          % "2.1.6",
      "dev.zio"    %% "zio-streams"  % "2.1.6",
      "dev.zio"    %% "zio-prelude"  % "1.0.0-RC27",
      "dev.zio"    %% "zio-parser"   % "0.1.10",
      "org.scalaz" %% "scalaz-core"  % "7.3.8", // For Dequeue
      "dev.zio"    %% "zio-test"     % "2.1.6" % Test,
      "dev.zio"    %% "zio-test-sbt" % "2.1.6" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    // Scalafix
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
