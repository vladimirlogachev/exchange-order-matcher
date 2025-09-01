addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.5")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.2")

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
