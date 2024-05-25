package exchange.infra.fileapi

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import exchange.domain.model._
import zio.parser._

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

val orderSideSyntax = Syntax.char('b').as(OrderSide.Buy) | Syntax.char('s').as(OrderSide.Sell)

val orderSyntax: Syntax[String, Char, Char, Order] = {
  val tupleSyntax =
    clientNameSyntax
      ~ tabChar
      ~ orderSideSyntax
      ~ tabChar
      ~ assetNameSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetPriceSyntax

  tupleSyntax.transformTo(
    Order.apply.tupled,
    x => (x.clientName, x.side, x.assetName, x.assetAmount, x.assetPrice),
    "Not an Order"
  ) ?? "Order"
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
