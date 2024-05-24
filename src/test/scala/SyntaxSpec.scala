import zio.test._

import ClientName._
import AssetName._
import UsdAmount._
import AssetAmount._
import AssetPrice._

object SyntaxSpec extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] =
    suite("Syntax")(
      test("ClientBalanceRecord") {
        val input = "C1	1000	10	5	15	0"

        val expectedOutput = for {
          usdBalance <- UsdAmount(1000)
          balanceA   <- AssetAmount(10)
          balanceB   <- AssetAmount(5)
          balanceC   <- AssetAmount(15)
          balanceD   <- AssetAmount(0)
        } yield ClientBalanceRecord(ClientName("C1"), usdBalance, balanceA, balanceB, balanceC, balanceD)

        val parsingResult        = ClientBalanceRecord.syntax.parseString(input).toOption
        val parsedAndPrintedBack = parsingResult.flatMap(x => ClientBalanceRecord.syntax.printString(x).toOption)

        assertTrue(parsingResult == expectedOutput)
        && assertTrue(parsedAndPrintedBack == Some(input))
      },
      test("ClientOrder Buy") {
        val input = "C1	b	A	10	12"

        val expectedOutput = for {
          usdAmount  <- UsdAmount(10)
          assetPrice <- AssetPrice(12)
        } yield ClientOrder.Buy(ClientName("C1"), AssetName("A"), usdAmount, assetPrice)

        val parsingResult = clientOrderSyntax.parseString(input).toOption

        assertTrue(parsingResult == expectedOutput)
      },
      test("ClientOrder Sell") {
        val input = "C2	s	A	8	10"

        val expectedOutput = for {
          assetAmount <- AssetAmount(8)
          assetPrice  <- AssetPrice(10)
        } yield ClientOrder.Sell(ClientName("C2"), AssetName("A"), assetAmount, assetPrice)

        val parsingResult = clientOrderSyntax.parseString(input).toOption

        assertTrue(parsingResult == expectedOutput)
      }
    )

}
