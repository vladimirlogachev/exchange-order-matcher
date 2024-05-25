# exchange-order-matcher

## Pre-requisites

- Sbt

## Usage

- `sbt run` - run
- `sbt test` - test
- `sbt styleFix` - fix formatting and linting errors
- `sbt styleCheck` - check for formatting and linting errors.

## Design

- `Matcher` – responsible for processing orders
- `FileApi` – a file-based API for the exchange
  (loading client balances to the state and back, interacting with matcher). Also knows about the string syntax.
- `Cli` – responsible for dealing with the file system and for interacting with the user.
