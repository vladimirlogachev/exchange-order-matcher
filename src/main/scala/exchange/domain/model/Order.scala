package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import zio.prelude.Equal

enum OrderSide:
  case Buy
  case Sell

final case class Order(
    clientName: ClientName,
    side: OrderSide,
    assetName: AssetName,
    assetAmount: AssetAmount,
    assetPrice: AssetPrice
) {
  def usdAmount: Option[UsdAmount] = assetAmount.toUsdAmount(assetPrice)
}

object Order:
  implicit val OrderEqual: Equal[Order] =
    Equal.default
