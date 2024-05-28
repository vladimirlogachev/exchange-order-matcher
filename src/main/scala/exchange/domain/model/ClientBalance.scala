package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.UsdAmounts._

final case class CompoundBalance[A](
    free: A,
    locked: A
)

object CompoundBalance:
  def totalUsdBalance(x: CompoundBalance[UsdAmount]) = x.free + x.locked

  def totalAssetBalance(x: CompoundBalance[AssetAmount]) = x.free + x.locked

final case class ClientBalance(
    usdBalance: CompoundBalance[UsdAmount],
    assetBalances: Map[AssetName, CompoundBalance[AssetAmount]]
)
