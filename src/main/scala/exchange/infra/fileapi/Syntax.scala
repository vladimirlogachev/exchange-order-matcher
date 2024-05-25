package exchange.infra.fileapi

import zio.parser._

import exchange.domain.model._
import ClientNames._
import AssetNames._
import UsdAmounts._
import AssetAmounts._
import AssetPrices._

val tabChar = Syntax.char('\t')

val clientNameSyntax: Syntax[String, Char, Char, ClientName] =
  Syntax.notChar('\t').repeat.string.transform(ClientName.apply, ClientName.unwrap)

val assetNameSyntax: Syntax[String, Char, Char, AssetName] =
  Syntax.notChar('\t').repeat.string.transform(AssetName.apply, AssetName.unwrap)

val usdAmountSyntax: Syntax[String, Char, Char, UsdAmount] = Syntax.digit.repeat.string.transformEither(
  _.toIntOption.flatMap(UsdAmount.apply(_)).toRight("Not a valid USD amount"),
  v => Right(v.toString)
) ?? "UsdAmount"

val assetAmountSyntax: Syntax[String, Char, Char, AssetAmount] = Syntax.digit.repeat.string.transformEither(
  _.toIntOption.flatMap(AssetAmount.apply(_)).toRight("Not a valid asset amount"),
  v => Right(v.toString)
) ?? "AssetAmount"

val assetPriceSyntax: Syntax[String, Char, Char, AssetPrice] = Syntax.digit.repeat.string.transformEither(
  _.toIntOption.flatMap(AssetPrice.apply(_)).toRight("Not a valid asset price"),
  v => Right(v.toString)
) ?? "AssetPrice"

val clientOrderSyntax: Syntax[String, Char, Char, ClientOrder] = {
  val buySyntax: Syntax[String, Char, Char, ClientOrder] = {
    val tupleSyntax =
      clientNameSyntax
        ~ tabChar
        ~ Syntax.char('b')
        ~ tabChar
        ~ assetNameSyntax
        ~ tabChar
        ~ usdAmountSyntax
        ~ tabChar
        ~ assetPriceSyntax

    tupleSyntax.transformTo(
      ClientOrder.Buy.apply.tupled,
      { case (x: ClientOrder.Buy) => (x.clientName, x.assetName, x.usdAmount, x.assetPrice) },
      "Not a Buy order"
    ) ?? "ClientOrder.Buy"
  }

  val sellSyntax: Syntax[String, Char, Char, ClientOrder] = {
    val tupleSyntax =
      clientNameSyntax
        ~ tabChar
        ~ Syntax.char('s')
        ~ tabChar
        ~ assetNameSyntax
        ~ tabChar
        ~ assetAmountSyntax
        ~ tabChar
        ~ assetPriceSyntax

    tupleSyntax.transformTo(
      ClientOrder.Sell.apply.tupled,
      { case (x: ClientOrder.Sell) => (x.clientName, x.assetName, x.assetAmount, x.assetPrice) },
      "Not a Sell order"
    ) ?? "ClientOrder.Sell"
  }

  (buySyntax | sellSyntax) ?? "ClientOrder"
}

val clientBalanceRecordSyntax = {
  val tupleSyntax =
    clientNameSyntax
      ~ tabChar
      ~ usdAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax

  tupleSyntax.transform(
    ClientBalanceRecord.apply.tupled,
    x => (x.clientName, x.usdBalance, x.balanceA, x.balanceB, x.balanceC, x.balanceD)
  ) ?? "ClientBalanceRecord"
}
