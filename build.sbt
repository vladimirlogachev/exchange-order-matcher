val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "exchange-order-matcher",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    run / fork   := true, // Makes exit codes work as expected
    // fp
    libraryDependencies ++= Seq(
      "dev.zio"    %% "zio"         % Versions.zio,
      "dev.zio"    %% "zio-streams" % Versions.zio,
      "dev.zio"    %% "zio-prelude" % Versions.zioPrelude,
      "dev.zio"    %% "zio-parser"  % Versions.zioParser,
      "org.scalaz" %% "scalaz-core" % Versions.scalaz // For Dequeue
    ),
    // tests
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test"     % Versions.zio,
      "dev.zio" %% "zio-test-sbt" % Versions.zio
    ).map(_ % Test),

    // Scalafix
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
