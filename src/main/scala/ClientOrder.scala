import zio.parser._

import ClientName._
import AssetName._
import UsdAmount._
import AssetAmount._
import AssetPrice._

enum ClientOrder:
  case Buy(
      clientName: ClientName,
      assetName: AssetName,
      usdAmount: UsdAmount,
      assetPrice: AssetPrice
  )
  case Sell(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount,
      assetPrice: AssetPrice
  )

val clientOrderSyntax: Syntax[String, Char, Char, ClientOrder] = {
  val buySyntax: Syntax[String, Char, Char, ClientOrder] = {
    val tupleSyntax =
      ClientName.syntax
        ~ tabChar
        ~ Syntax.char('b')
        ~ tabChar
        ~ AssetName.syntax
        ~ tabChar
        ~ UsdAmount.syntax
        ~ tabChar
        ~ AssetPrice.syntax

    tupleSyntax.transformTo(
      ClientOrder.Buy.apply.tupled,
      { case (x: ClientOrder.Buy) => (x.clientName, x.assetName, x.usdAmount, x.assetPrice) },
      "Not a Buy order"
    ) ?? "ClientOrder.Buy"
  }

  val sellSyntax: Syntax[String, Char, Char, ClientOrder] = {
    val tupleSyntax =
      ClientName.syntax
        ~ tabChar
        ~ Syntax.char('s')
        ~ tabChar
        ~ AssetName.syntax
        ~ tabChar
        ~ AssetAmount.syntax
        ~ tabChar
        ~ AssetPrice.syntax

    tupleSyntax.transformTo(
      ClientOrder.Sell.apply.tupled,
      { case (x: ClientOrder.Sell) => (x.clientName, x.assetName, x.assetAmount, x.assetPrice) },
      "Not a Sell order"
    ) ?? "ClientOrder.Sell"
  }

  (buySyntax | sellSyntax) ?? "ClientOrder"
}
