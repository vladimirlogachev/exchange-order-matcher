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

type OrderBook = Vector[Order]

final case class ExchangeState(
    balances: Map[ClientName, ClientBalances], // TODO: locked assets and usd
    orders: Map[AssetName, OrderBook]
)

object ExchangeState:

  def empty: ExchangeState = ExchangeState(
    balances = Map.empty,
    orders = Map(
      AssetName("A") -> Vector.empty,
      AssetName("B") -> Vector.empty,
      AssetName("C") -> Vector.empty,
      AssetName("D") -> Vector.empty
    )
  )

enum OrderRejectionReason:
  case ClientNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance
  case UnexpectedInternalError // TODO: consider removing or defining more specific errors

implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
  Equal.default

object Exchange:

  def processOrder(
      order: Order,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = order.side match {
    case OrderSide.Buy =>
      for {
        requiredUsdAmount <- order.assetAmount
          .toUsdAmount(order.assetPrice)
          .toRight(OrderRejectionReason.UnexpectedInternalError)
        state1 <- lockUsd(order.clientName, requiredUsdAmount, state)
        // orderBook <- state.orders.get(order.assetName).toRight(OrderRejectionReason.TodoError)
      } yield state1
    case OrderSide.Sell =>
      for {
        state1 <- lockAsset(order.clientName, order.assetName, order.assetAmount, state)
      } yield state1
  }

  /** TODO: maybe add orderId to lock assets for? */
  private def lockUsd(
      clientName: ClientName,
      usdAmount: UsdAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalances <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    _ <-
      if clientBalances.usdBalance >= usdAmount then Right(()) // TODO: lock
      else Left(OrderRejectionReason.InsufficientUsdBalance)
  } yield state

  /** TODO: maybe add orderId to lock assets for? */
  private def lockAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalances <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance   <- clientBalances.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    _ <-
      if assetBalance >= assetAmount then Right(()) // TODO: lock
      else Left(OrderRejectionReason.InsufficientAssetBalance)
  } yield state
