package exchange.infra.fileapi

import exchange.domain.model._
import zio._
import zio.prelude._
import zio.stream._
import zio.test._

object FileApiSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] = suite("Simle cases")(
    /* ------------------- Example cases, "as is", just in case ------------------- */
    test("Provided example 1: from file format description") {
      val clientBalances = ZStream("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
      // Note: the only sell order is rejected because of insufficient asset balance
      val orders                = ZStream("C1	b	A	10	12", "C2	s	A	8	10")
      val expectedFinalBalances = Set("C1	1000	10	5	15	0", "C2	2000	3	35	40	10")
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
    test("Buy order with insufficient USD balance leads to an error") {
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
    test("Sell order with insufficient asset balance leads to an error") {
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
        assertTrue(outputEither == Right(expectedFinalBalances))
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
    }
  )

}
