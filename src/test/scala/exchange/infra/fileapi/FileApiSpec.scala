package exchange.infra.fileapi

import zio._
import zio.prelude.Equal
import zio.prelude.EqualOps
import zio.stream._
import zio.test._

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.ExchangeState
import exchange.domain.model.Order
import exchange.domain.model.OrderAmounts._
import exchange.domain.model.OrderRejectionReason
import exchange.domain.model.OrderSide
import exchange.domain.model.UsdAmounts._

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
      val expectedFinalBalances = Set("C1	900	30	20	20	20", "C2	950	25	20	20	20", "C3	1150	5	20	20	20")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    /* ------------------- BigInt ------------------- */
    test("Supports big values for balances") {
      val clientBalances = ZStream(
        "C1	10000000000000000	10000000000000000	0	0	0",
        "C2	10000000000000000	10000000000000000	0	0	0"
      )
      val orders = ZStream("C1	b	A	2	5", "C2	s	A	1	5", "C2	s	A	1	5")
      val expectedFinalBalances = Set(
        "C1	9999999999999990	10000000000000002	0	0	0",
        "C2	10000000000000010	9999999999999998	0	0	0"
      )
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Supports big values for order price") {
      val clientBalances = ZStream(
        "C1	10000000000000000	10000000000000000	0	0	0",
        "C2	10000000000000000	10000000000000000	0	0	0"
      )
      val orders = ZStream("C1	b	A	2	500000000000000", "C2	s	A	1	500000000000000", "C2	s	A	1	500000000000000")
      val expectedFinalBalances = Set(
        "C1	9000000000000000	10000000000000002	0	0	0",
        "C2	11000000000000000	9999999999999998	0	0	0"
      )
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Supports big values for order amount") {
      val clientBalances = ZStream(
        "C1	10000000000000000	10000000000000000	0	0	0",
        "C2	10000000000000000	10000000000000000	0	0	0"
      )
      val orders = ZStream("C1	b	A	200000000000000	5", "C2	s	A	100000000000000	5", "C2	s	A	100000000000000	5")
      val expectedFinalBalances = Set(
        "C1	9000000000000000	10200000000000000	0	0	0",
        "C2	11000000000000000	9800000000000000	0	0	0"
      )
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
    test("Order of an unknown client is rejected") {
      val clientBalances         = ZStream("C1	100	10	10	10	10")
      val orders                 = ZStream("C2	s	B	1	5")
      val expectedRejectedOrders = Vector(("C2	s	B	1	5", OrderRejectionReason.ClientNotFound))
      for {
        outputEither <- FileApi.runFromStrings(clientBalances, orders).either
      } yield {
        val rejectedOrders = outputEither.flatMap(x => FileApi.toSimplifiedRejectedOrders(x))
        assertTrue(rejectedOrders === Right(expectedRejectedOrders))
      }
    },
    test("Order with an unknown asset is rejected") {
      val clientBalances         = ZStream("C1	100	10	10	10	10")
      val orders                 = ZStream("C1	s	X	1	5")
      val expectedRejectedOrders = Vector(("C1	s	X	1	5", OrderRejectionReason.UnknownAsset))
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
    },
    test("Same client, 1 buy order, then 2 sell orders, same total amount, same price, balance unchganged") {
      val clientBalances        = ZStream("C1	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	2	5", "C1	s	A	1	5", "C1	s	A	1	5")
      val expectedFinalBalances = Set("C1	100	10	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Same client, 2 buy orders, then 1 sell order, same total amount, same price, balance unchganged") {
      val clientBalances        = ZStream("C1	100	10	10	10	10")
      val orders                = ZStream("C1	b	A	1	5", "C1	b	A	1	5", "C1	s	A	2	5")
      val expectedFinalBalances = Set("C1	100	10	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Same client, 1 sell order, then 2 buy orders, same total amount, same price, balance unchganged") {
      val clientBalances        = ZStream("C1	100	10	10	10	10")
      val orders                = ZStream("C1	s	A	2	5", "C1	b	A	1	5", "C1	b	A	1	5")
      val expectedFinalBalances = Set("C1	100	10	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    },
    test("Same client, 2 sell orders, then 1 buy order, same total amount, same price, balance unchganged") {
      val clientBalances        = ZStream("C1	100	10	10	10	10")
      val orders                = ZStream("C1	s	A	1	5", "C1	s	A	1	5", "C1	b	A	2	5")
      val expectedFinalBalances = Set("C1	100	10	10	10	10")
      for {
        outputEither <- FileApi.runFromStringsToStrings(clientBalances, orders).either
      } yield {
        assertTrue(outputEither === Right(expectedFinalBalances))
      }
    }
  )

}

object PropertiesSpec extends ZIOSpecDefault {

  val genBigInt =
    Gen.oneOf(
      Gen.fromIterable(List(BigInt("1"))),
      Gen.bigInt(BigInt("2"), BigInt("999")),
      Gen.bigInt(BigInt("999"), BigInt("999999")),
      Gen.bigInt(BigInt("999999"), BigInt("9999999999"))
    )

  val genUsdAmount = genBigInt.map(UsdAmount(_).get)

  val genAssetAmount = genBigInt.map(AssetAmount(_).get)

  val genOrderAmount = genBigInt.map(OrderAmount(_).get)

  val genAssetPrice = genBigInt.map(AssetPrice(_).get)

  val genOrderSide = Gen.elements(OrderSide.Buy, OrderSide.Sell)

  /** Generates one of these values, could be in any order and/or repeated */
  val genClientNameForOrders: Gen[Any, ClientName] =
    Gen.elements("C1", "C2", "C3", "C4", "C5").map(ClientName(_))

  val genAssetName: Gen[Any, AssetName] = Gen.fromIterable(List("A", "B", "C", "D")).map(AssetName(_))

  val genClientBalanceValues: Gen[Any, (UsdAmount, AssetAmount, AssetAmount, AssetAmount, AssetAmount)] =
    for {
      usdBalance <- genUsdAmount
      balanceA   <- genAssetAmount
      balanceB   <- genAssetAmount
      balanceC   <- genAssetAmount
      balanceD   <- genAssetAmount
    } yield (usdBalance, balanceA, balanceB, balanceC, balanceD)

  /* Generates exactly these 5 clients */
  val genClients = {
    for {
      client1 <- genClientBalanceValues.map(x => ClientBalanceRecord(ClientName("C1"), x._1, x._2, x._3, x._4, x._5))
      client2 <- genClientBalanceValues.map(x => ClientBalanceRecord(ClientName("C2"), x._1, x._2, x._3, x._4, x._5))
      client3 <- genClientBalanceValues.map(x => ClientBalanceRecord(ClientName("C3"), x._1, x._2, x._3, x._4, x._5))
      client4 <- genClientBalanceValues.map(x => ClientBalanceRecord(ClientName("C4"), x._1, x._2, x._3, x._4, x._5))
      client5 <- genClientBalanceValues.map(x => ClientBalanceRecord(ClientName("C5"), x._1, x._2, x._3, x._4, x._5))
    } yield List(client1, client2, client3, client4, client5)
  }

  val genOrder = for {
    clientName <- genClientNameForOrders
    side       <- genOrderSide
    assetName  <- genAssetName
    amount     <- genOrderAmount
    price      <- genAssetPrice
  } yield Order(clientName, side, assetName, amount, price)

  def spec = suite("Known invariants")(
    test(
      """|The sum of all client balances of USD and of every individual asset
         |is the same before and after processing any orders""".stripMargin
    ) {
      check(genClients, Gen.listOfN(1000)(genOrder)) { (listOfBalances, listOfOrders) =>
        {
          val clientBalances = ZStream.fromIterable(listOfBalances)
          val orders         = ZStream.fromIterable(listOfOrders)

          for {
            outputEither1 <- FileApi.run(clientBalances, ZStream.empty).either
            totalBeforeOrders = outputEither1.map(x => x.state.clientBalanceTotal)

            outputEither2 <- FileApi.run(clientBalances, orders).either
            totalAfterOrders = outputEither2.map(_.state.clientBalanceTotal)
          } yield {
            assertTrue(totalBeforeOrders == totalAfterOrders) // scalafix:ok
          }
        }
      }
    }
  )

}
