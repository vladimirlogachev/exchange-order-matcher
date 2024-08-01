# exchange-order-matcher

Coding assessment (Scala 3, ZIO).

Completed in 2024, and currently maintained as a demonstration project.

## Pre-requisites

- [sbt](https://www.scala-sbt.org/)

## Usage

The program reads the inputs from the `orders.txt` and `clients.txt` files and writes the output to the `results.txt` file.

- `sbt run` - run
- `sbt test` - test
- `sbt styleFix` - fix formatting and linting errors
- `sbt styleCheck` - check for formatting and linting errors
- `sbt dev` - allow compiler warnings to not fail the compilation

## Design

- `ExchangeState` – responsible for processing orders
- `FileApi` – a file-based API for the exchange
  (loading client balances to the state and back, interacting with matcher). Also knows about the string syntax.
- `Cli` – responsible for dealing with the file system and interacting with the user.

## Design Choices

- All types and data structures are immutable.
- `OrderAmount` and `AssetPrice` are forced to be positive (and never zero) using the opaque types.
  - This means that orders with zero amount or zero price are rejected not because they are invalid from the `ExchangeState` perspective, but from a parsing perspective. This means, that if `orders.txt` contains an order with zero amount or zero price, the program will exit with `ExitCode.failure`. In a real-world scenario, we won't want to accept such orders from customers, and reject them without submitting, which also fits the current design choice.
  - The alternative could be to allow zero amounts and prices, but this would make the core logic less reliable.
- The task assumes that there are only 4 available assets: `A`, `B`, `C`, and `D`, but the parser doesn't restrict the `AssetName` syntax to these 4. Instead, the invalid assetName will be rejected by the `ExchangeState.processOrder` with the `OrderRejectionReason.UnknownAsset` error.
- Similarly, `AssetAmount` and `UsdAmount` (used for client balances and most computations) are forced to be non-negative.
- Every customer balance is represented via 2 parts: `free` and `locked`. Functions that manage the `OrderBook` are implemented in such a way that they automatically update locked and free balances every time the order is inserted or removed. This way, in any given `ExchangeState` locked client balances are expected to be consistent with the `OrderBook`.
  - This choice means that in order to work with the existing order from the book, it is removed from the queue, and then the remaining part could be put back, and the updated state doesn't contain such `temporarily removed` orders.
  - The `free` (not the `total`) balance is used to check if the client has sufficient funds for the order. By locking the funds for the orders in the order book, we guarantee that once the order is placed, the client has enough funds to execute it.
- Order price is a so-called "limit price". The order is executed at the best available price, but not worse than the limit price.
  - A better price can be used if there are orders in the order book with a better price than specified in the order.
  - After all matching orders are used, and the order is placed in the order book, only its specified limit price will be used for trades.
- All errors are represented as ADTs, and the program never throws exceptions, but uses error handling capabilities of `Either`, `ZIO`, and `ZStream` instead.
- There is a `UnexpectedInternalError` variant in the `UnexpectedInternalError` ADT. It corresponds to errors that must never happen as long as the input data is correctly parsed (which it is expected to be).
  - Currently, such error variant doesn't specific error message, because there's nothing to explain to the user. 
  - In real-world scenario, such error could contain technical details for the developers. However, the decision is to leave it outside of the scope of this task.
- Loading initial balances into the `ExchangeState` is considered a part of the `FileApi`, because it is a simplified file-based way to set the balances.
  - In a real-world scenario, we would prefer the core to work with the deposit and withdrawal operations instead of allowing the client code to simply set the balance.

### Known Invariants

- The sum of all client balances of USD and of every individual asset is the same before and after processing any orders (because there are no exchange fees).
- The sum of locked assets is equal to the sum of order amounts in the order book.
