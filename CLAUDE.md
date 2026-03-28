# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Development Commands

Build tool is **sbt** (v1.10.4). Scala 3.5.2.

- `sbt compile` — compile
- `sbt run` — run (reads `clients.txt` and `orders.txt`, writes `results.txt`)
- `sbt test` — run all tests (ZIO Test framework)
- `sbt styleFix` — auto-fix formatting (scalafmt) and linting (scalafix)
- `sbt styleCheck` — check formatting and linting without fixing
- `sbt dev` — switch to dev mode (warnings don't fail compilation)
- `sbt ci` — switch to CI mode (strict, warnings are errors)

CI runs: `sbt ci compile styleCheck test`

There is no single-test command built in; use `sbt "testOnly exchange.infra.fileapi.FileApiSpec"` to run a specific spec.

## Architecture

Three layers, all purely functional (ZIO, no exceptions, no mutable state):

1. **Domain** (`exchange.domain.model`) — pure business logic, no side effects
   - `ExchangeState` — the core order matching state machine. Processes orders via `processOrder()`, which delegates to tail-recursive `buyRecursive`/`sellRecursive` matching loops.
   - `OrderBook` — TreeMap (price-ordered) + Dequeue (FIFO at same price level)
   - `ClientBalance` — compound balance with `free` (available) and `locked` (reserved by open orders) parts
   - `Types` — opaque types enforcing invariants at compile time: `OrderAmount`/`AssetPrice` (must be > 0), `AssetAmount`/`UsdAmount` (must be >= 0)
   - `Errors` — `OrderRejectionReason` ADT (never throws)

2. **FileApi** (`exchange.infra.fileapi`) — bridges domain and file I/O
   - `FileApi` — orchestrates loading balances, processing orders, serializing results
   - `Syntax` — ZIO Parser combinators for tab-separated input/output format

3. **CLI** (`exchange.infra.cli.Cli`) — ZIOAppDefault entry point, file system interaction

## Key Design Decisions

- **Opaque types** enforce constraints (positive amounts, non-negative balances) — invalid values are rejected at parse time, not in domain logic.
- **Free/locked balance split** — placing an order locks funds; the order book and balances stay consistent automatically.
- **Limit price semantics** — orders execute at the best available price, never worse than the specified limit.
- **Key invariant**: total USD and each asset's total across all clients is preserved after processing (no exchange fees). This is verified by property-based tests.

## Code Quality Rules

- **Scalafix** enforces: no vars, throws, nulls, returns, while loops, `asInstanceOf`/`isInstanceOf`, universal equality (`==`/`!=` — use ZIO Prelude `Equal` instead)
- **Scalafmt**: max 120 columns, Scala 3 dialect, aligned imports
- Always run `sbt styleFix` before committing
