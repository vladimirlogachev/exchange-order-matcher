package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import zio.prelude._

enum OrderSide:
  case Buy
  case Sell

final case class Order(
    clientName: ClientName,
    side: OrderSide,
    assetName: AssetName,
    assetAmount: AssetAmount,
    assetPrice: AssetPrice
)

object Order:

  implicit val OrderEqual: Equal[Order] =
    Equal.default

final case class ClientBalances(
    usdBalance: UsdAmount,
    assetBalances: Map[AssetName, AssetAmount]
)

final case class ExchangeState(
    balances: Map[ClientName, ClientBalances], // TODO: locked assets and usd
    pendingOrders: List[Order]                 // TODO: not a list
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
  case TodoError // TODO: remove

implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
  Equal.default

object Exchange:

  def processOrder(
      order: Order,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = order.side match {
    case OrderSide.Buy =>
      for {
        clientBalances    <- state.balances.get(order.clientName).toRight(OrderRejectionReason.ClientNotFound)
        requiredUsdAmount <- order.assetAmount.toUsdAmount(order.assetPrice).toRight(OrderRejectionReason.TodoError)

        _ <-
          if clientBalances.usdBalance >= requiredUsdAmount then Right(())
          else Left(OrderRejectionReason.InsufficientUsdBalance)
        // TODO: implement the rest
      } yield state.copy(pendingOrders = order :: state.pendingOrders)
    case OrderSide.Sell =>
      for {
        clientBalances <- state.balances.get(order.clientName).toRight(OrderRejectionReason.ClientNotFound)
        assetBalance <- clientBalances.assetBalances
          .get(order.assetName)
          .toRight(OrderRejectionReason.AssetBalanceNotFound)
        _ <-
          if assetBalance >= order.assetAmount then Right(()) else Left(OrderRejectionReason.InsufficientAssetBalance)
        // _              <- Right(println(clientBalances.usdBalance))
        // _              <- Right(println(usdAmount))
        // TODO: implement the rest
      } yield state.copy(pendingOrders = order :: state.pendingOrders)
  }
