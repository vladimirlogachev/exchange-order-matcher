package exchange.infra.fileapi

import exchange.domain.model._
import zio._
import zio.prelude._
import zio.stream._
import zio.test._

object FileApiSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] = suite("Simle cases")(
    /* ------------------- Example cases, "as is", just in case ------------------- */
    test("Provided example 1: file format description") {
      val clientBalances = ZStream("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      val orders         = ZStream("C1	b	A	10	12", "C2	s	A	8	10")
      // Note: The only sell order is rejected because of insufficient asset balance.
      // So, balances are unchanged.
      val expectedFinalBalances = Set("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Provided example 2: rejected orders with insufficient client balances") {
      val clientBalances = ZStream("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      val orders         = ZStream("C1	b	A	10	150", "C2	s	B	40	10")
      // Note: Order 1 is rejected because C1 does not have enough USD to buy 10 of "A" assets at $150.
      // Order 2 is rejected because C2 has only the amount 35 of a "B" asset.
      // So, balances are unchanged.
      val expectedFinalBalances = Set("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Provided example 3: partial order filling") {
      val clientBalances = ZStream("C1	1000	10	10	10	10", "C2	1000	10	10	10	10")
      val orders         = ZStream("C1	b	A	10	12", "C2	s	A	8	10")
      // Note: Orders 1 and 2 are partially matched:
      // Order 2 is completely filled,
      // Order 1 is partially filled and its state changed to `C1	b	A	2	12`.
      // TODO: Check the remaining order in the orderBook, convert orderBook to Vector[String]
      val expectedFinalBalances = Set("C1	904	18	10	10	10", "C2	1096	2	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Provided example 4: Order fillin order: by price, and then by time (earliest first)") {
      val clientBalances = ZStream("C1	1000	20	20	20	20", "C2	1000	20	20	20	20", "C3	1000	20	20	20	20")
      val orders         = ZStream("C1	b	A	10	10", "C2	b	A	10	10", "C3	s	A	15	10")

      // Note: Orders 3 and 1 will be matched first, Order 1 will be fully settled
      // Then the remainder of Order 3 will be matched with order 2.
      // TODO: Check the remaining order in the orderBook, convert orderBook to Vector[String]
      val expectedFinalBalances = Set("C1	900	30	20	20	20", "C2	950	25	20	20	20", "C3	1150	5	20	20	20")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    /* ------------------- Client balances causing errors ------------------- */
    test("Duplicate client name leads to an error") {
      val clientBalances = ZStream("C1	1000	10	5	15	0", "C2	2000	3	35	40	10", "C1	500	3	35	40	10")
      val orders         = ZStream.empty
      val expectedError  = StringFileApiError.ItsFileApiError(FileApiError.ClientAlreadyExists)
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Left(expectedError))
      }
    },

    /* ------------------- Orders being rejected ------------------- */
    test("Buy order of client with insufficient USD balance is rejected") {
      val clientBalances         = ZStream("C1	1000	10	5	15	0")
      val orders                 = ZStream("C1	b	A	10	150")
      val expectedRejectedOrders = Vector(("C1	b	A	10	150", OrderRejectionReason.InsufficientUsdBalance))
      for {
        outputEither <- FileApi.runFromStrings(clientBalances, orders).either
      } yield {
        val rejectedOrders = outputEither.flatMap(x => FileApi.toSimplifiedRejectedOrders(x))
        assertTrue(rejectedOrders === Right(expectedRejectedOrders))
      }
    },
    test("Sell order of client with insufficient asset balance is rejected") {
      val clientBalances         = ZStream("C2	2000	3	35	40	10")
      val orders                 = ZStream("C2	s	B	40	10")
      val expectedRejectedOrders = Vector(("C2	s	B	40	10", OrderRejectionReason.InsufficientAssetBalance))
      for {
        outputEither <- FileApi.runFromStrings(clientBalances, orders).either
      } yield {
        val rejectedOrders = outputEither.flatMap(x => FileApi.toSimplifiedRejectedOrders(x))
        assertTrue(rejectedOrders === Right(expectedRejectedOrders))
      }
    },
    test("Buy order with zero assetAmount is rejected") {
      val clientBalances         = ZStream("C1	100	10	10	10	10")
      val orders                 = ZStream("C1	b	A	0	5")
      val expectedRejectedOrders = Vector(("C1	b	A	0	5", OrderRejectionReason.InvalidAssetAmount))
      for {
        outputEither <- FileApi.runFromStrings(clientBalances, orders).either
      } yield {
        val rejectedOrders = outputEither.flatMap(x => FileApi.toSimplifiedRejectedOrders(x))
        assertTrue(rejectedOrders === Right(expectedRejectedOrders))
      }
    },
    test("Sell order with zero assetAmount is rejected") {
      val clientBalances         = ZStream("C1	100	10	10	10	10")
      val orders                 = ZStream("C1	s	A	0	5")
      val expectedRejectedOrders = Vector(("C1	s	A	0	5", OrderRejectionReason.InvalidAssetAmount))
      for {
        outputEither <- FileApi.runFromStrings(clientBalances, orders).either
      } yield {
        val rejectedOrders = outputEither.flatMap(x => FileApi.toSimplifiedRejectedOrders(x))
        assertTrue(rejectedOrders === Right(expectedRejectedOrders))
      }
    },
    /* ------------------- Orders not being filled ------------------- */

    test("Empty inputs produce empty outputs") {
      val expectedOutput = FileApiOutput(
        state = ExchangeState.empty,
        rejectedOrders = Vector.empty
      )
      for {
        out <- FileApi.runFromStrings(ZStream.empty, ZStream.empty).either
      } yield assertTrue(out === Right(expectedOutput))
    },
    /* ------------------- 1 order can be filled by 1 other, same price ------------------- */
    test("Full order execution, 1 buy order, then 1 sell order, same amount, same price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	2	5", "C2	s	A	2	5")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Full order execution, 1 sell order, then 1 buy order, same amount, same price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C2	s	A	2	5", "C1	b	A	2	5")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    /* ------------------- 1 order can be filled by 2 others, same price ------------------- */
    test("Full order execution, 1 buy order, then 2 sell orders, same total amount, same price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	2	5", "C2	s	A	1	5", "C2	s	A	1	5")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Full order execution, 2 buy orders, then 1 sell order, same total amount, same price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	1	5", "C1	b	A	1	5", "C2	s	A	2	5")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Full order execution, 1 sell order, then 2 buy orders, same total amount, same price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C2	s	A	2	5", "C1	b	A	1	5", "C1	b	A	1	5")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Full order execution, 2 sell orders, then 1 buy order, same total amount, same price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C2	s	A	1	5", "C2	s	A	1	5", "C1	b	A	2	5")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    /* ------------------- Limit orders can lead to execution on different price ------------------- */
    test("Full order execution, 1 buy order, then 1 sell order, same amount, different price (price from the book)") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	2	5", "C2	s	A	2	4")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Full order execution, 1 sell order, then 1 buy order, same amount, different price (price from the book)") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C2	s	A	2	5", "C1	b	A	2	6")
      val expectedFinalBalances = Set("C1	90	12	10	10	10", "C2	110	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    /* ------------------- Limit orders are filled with trades of different price ------------------- */
    test("Full order execution, 2 buy orders, then 1 sell order, same total amount, different price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	1	7", "C1	b	A	1	6", "C2	s	A	2	5")
      val expectedFinalBalances = Set("C1	87	12	10	10	10", "C2	113	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Full order execution, 2 sell orders, then 1 buy order, same total amount, different price") {
      val clientBalances        = ZStream("C1	100	10	10	10	10", "C2	100	10	10	10	10")
      val orders                = ZStream("C2	s	A	1	3", "C2	s	A	1	2", "C1	b	A	2	5")
      val expectedFinalBalances = Set("C1	95	12	10	10	10", "C2	105	8	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    /* ------------------- When client sells anything to themselves, the balance remains unchanged ------------------- */
    test("Same client, same amount, same price, 1 buy -> 1 sell, balance unchganged") {
      val clientBalances        = ZStream("C1	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	2	5", "C1	s	A	2	5")
      val expectedFinalBalances = Set("C1	100	10	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Same client, same amount, same price, 1 buy -> 1 sell, balance unchganged") {
      val clientBalances        = ZStream("C1	100	10	10	10	10")
      val orders                = ZStream("C1	s	A	2	5", "C1	s	A	2	5")
      val expectedFinalBalances = Set("C1	100	10	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    }
  )

}
