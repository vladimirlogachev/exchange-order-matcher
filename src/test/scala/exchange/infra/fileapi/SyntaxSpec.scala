package exchange.infra.fileapi

import zio.prelude._
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
    }
  )

}
