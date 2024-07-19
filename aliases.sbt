addCommandAlias(
  "styleCheck",
  "scalafixAll --check; scalafmtSbtCheck; scalafmtCheckAll"
)

addCommandAlias(
  "styleFix",
  "scalafixAll; scalafmtSbt; scalafmtAll"
)

addCommandAlias(
  "dev",
  "tpolecatDevMode"
)

addCommandAlias(
  "ci",
  "tpolecatCiMode"
)
