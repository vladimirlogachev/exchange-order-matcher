addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.12.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.1")

// Note: Optional plugins, left commented out to avoid unnecessary dependencies and/or conflicts

// addDependencyTreePlugin
// Usage:
//   sbt dependencyTree

// addSbtPlugin("org.jmotor.sbt" % "sbt-dependency-updates" % "1.2.9")
// Usage:
//   sbt dependencyUpdates

// addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.3.1")
// Usage:
//   sbt unusedCompileDependencies
//   sbt undeclaredCompileDependencies
