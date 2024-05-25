package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
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

final case class ClientBalances(
    usdBalance: UsdAmount,
    assetBalances: Map[AssetName, AssetAmount]
)

final case class MatcherState(
    balances: Map[ClientName, ClientBalances], // TODO: locked assets and usd
    pendingOrders: List[ClientOrder]           // TODO: not a list
)

object MatcherState:
  def empty: MatcherState = MatcherState(balances = Map.empty, pendingOrders = List.empty)

enum OrderRejectionReason:
  case ClientNotFound
  // TODO: try to get rid of it, e.g.:
  // InsufficientAssetBalance when failed to sell
  // create not found for buying
  case AssetBalanceNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance

def processOrder(
    clientOrder: ClientOrder,
    state: MatcherState
): Either[OrderRejectionReason, MatcherState] = clientOrder match {
  case ClientOrder.Buy(clientName, assetName, usdAmount, assetPrice) =>
    for {
      clientBalances <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
      assetBalance   <- clientBalances.assetBalances.get(assetName).toRight(OrderRejectionReason.AssetBalanceNotFound)
      _ <-
        if clientBalances.usdBalance >= usdAmount then Right(())
        else Left(OrderRejectionReason.InsufficientUsdBalance)
      // TODO: implement the rest
    } yield state
  case ClientOrder.Sell(clientName, assetName, assetAmount, assetPrice) =>
    for {
      clientBalances <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
      assetBalance   <- clientBalances.assetBalances.get(assetName).toRight(OrderRejectionReason.AssetBalanceNotFound)
      _ <- if assetBalance >= assetAmount then Right(()) else Left(OrderRejectionReason.InsufficientAssetBalance)
      // TODO: implement the rest
    } yield state
}
