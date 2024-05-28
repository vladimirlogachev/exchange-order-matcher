package exchange.infra.fileapi

import zio.prelude.Equal
import zio.prelude.EqualOps
import zio.test._

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.Order
import exchange.domain.model.OrderAmounts._
import exchange.domain.model.OrderSide
import exchange.domain.model.UsdAmounts._
import exchange.infra.fileapi._

object SyntaxSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] = suite("Syntax")(
    /* ------------------- Simple successful cases ------------------- */
    test("ClientBalanceRecord") {
      val input = "C1	1000	10	5	15	0"
      val expectedOutput = ClientBalanceRecord(
        ClientName("C1"),
        UsdAmount(1000).get,
        AssetAmount(10).get,
        AssetAmount(5).get,
        AssetAmount(15).get,
        AssetAmount(0).get
      )
      val parsingResult        = clientBalanceRecordSyntax.parseString(input).toOption
      val parsedAndPrintedBack = parsingResult.flatMap(x => clientBalanceRecordSyntax.printString(x).toOption)
      assertTrue(parsingResult === Some(expectedOutput))
      && assertTrue(parsedAndPrintedBack === Some(input))
    },
    test("Buy Order") {
      val input = "C1	b	A	10	12"
      val expectedOutput =
        Order(ClientName("C1"), OrderSide.Buy, AssetName("A"), OrderAmount(10).get, AssetPrice(12).get)
      val parsingResult = orderSyntax.parseString(input).toOption
      assertTrue(parsingResult === Some(expectedOutput))
    },
    test("Sell Order") {
      val input = "C2	s	A	8	10"
      val expectedOutput =
        Order(ClientName("C2"), OrderSide.Sell, AssetName("A"), OrderAmount(8).get, AssetPrice(10).get)
      val parsingResult = orderSyntax.parseString(input).toOption
      assertTrue(parsingResult === Some(expectedOutput))
    },
    /* ------------------- Orders being rejected ------------------- */
    test("An order with zero price is rejected") {
      val input = "C2	s	A	8	0"
      val expectedError =
        """|Failed to parse: Order.AssetPrice
           |Position: 10
           |Reason: Not a valid asset price""".stripMargin
      val parsingResult = orderSyntax.parseString(input).left.map(explainParserError)
      assertTrue(parsingResult === Left(expectedError))
    },
    test("An order with zero price is rejected") {
      val input = "C2	s	A	0	8"
      val expectedError =
        s"""|Failed to parse: Order.OrderAmount
            |Position: 8
            |Reason: Not a valid order amount""".stripMargin
      val parsingResult = orderSyntax.parseString(input).left.map(explainParserError)
      assertTrue(parsingResult === Left(expectedError))
    },
    test("An order with an invalid type is rejected") {
      val input = "C2	why	A	8	8"
      val expectedError =
        s"""|Reason: All branches failed
            |Left:
            |Failed to parse: Order.OrderSide
            |Position: 3
            |Reason: not 'b'
            |
            |Right:
            |Failed to parse: Order.OrderSide
            |Position: 3
            |Reason: not 's'""".stripMargin
      val parsingResult = orderSyntax.parseString(input).left.map(explainParserError)
      assertTrue(parsingResult === Left(expectedError))
    }
  )

}
