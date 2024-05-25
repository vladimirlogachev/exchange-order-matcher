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

final case class ExchangeState(
    balances: Map[ClientName, ClientBalances], // TODO: locked assets and usd
    pendingOrders: List[ClientOrder]           // TODO: not a list
)

object ExchangeState:
  def empty: ExchangeState = ExchangeState(balances = Map.empty, pendingOrders = List.empty)

enum OrderRejectionReason:
  case ClientNotFound
  // TODO: try to get rid of it, because it can't be caused by invalid order string, and so considered an internal error.
  // Maybe use other errors, e.g.:
  // InsufficientAssetBalance when failed to sell
  // InsufficientUsdBalance when failed to buy
  // or rename to
  // InternalErrorBalanceNotFoundShouldNeverHappen
  // or consider missing balance record as zero balance (which is pretty normal) and default to 0
  // or define getters and setters which default to 0
  case AssetBalanceNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance

def processOrder(
    order: ClientOrder,
    state: ExchangeState
): Either[OrderRejectionReason, ExchangeState] = order match {
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
