import exchange.domain.model._

import AssetAmounts._
import AssetNames._
import AssetPrices._
import ClientNames._
import UsdAmounts._

import zio.prelude._

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

implicit val ClientOrderEqual: Equal[ClientOrder] =
  Equal.default
