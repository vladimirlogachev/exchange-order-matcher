package exchange.domain.model

import zio.prelude.Equal

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.OrderAmounts._
import exchange.domain.model.UsdAmounts._

enum OrderSide:
  case Buy
  case Sell

final case class Order(
    clientName: ClientName,
    side: OrderSide,
    assetName: AssetName,
    amount: OrderAmount,
    assetPrice: AssetPrice
) {
  def usdAmount: Option[UsdAmount] = amount.toAssetAmount.toUsdAmount(assetPrice)
}

object Order:
  implicit val OrderEqual: Equal[Order] =
    Equal.default
