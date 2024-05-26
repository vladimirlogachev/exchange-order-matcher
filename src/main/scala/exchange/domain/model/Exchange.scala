package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import zio.prelude.Equal
import scala.collection.immutable.TreeMap
import scalaz.Dequeue

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

final case class CompoundBalance[A](
    free: A,
    locked: A
)

/** TODO: Consider separate CompoundBalance types */
def totalUsdBalance(x: CompoundBalance[UsdAmount]) = x.free + x.locked

def lockUsd(
    usdAmount: UsdAmount,
    b: CompoundBalance[UsdAmount]
): Either[OrderRejectionReason, CompoundBalance[UsdAmount]] = for {
  newFree <- (b.free - usdAmount).toRight(OrderRejectionReason.InsufficientUsdBalance)
} yield CompoundBalance(free = newFree, locked = b.locked + usdAmount)

def totalAssetBalance(x: CompoundBalance[AssetAmount]) = x.free + x.locked

def lockAsset(
    assetAmount: AssetAmount,
    b: CompoundBalance[AssetAmount]
): Either[OrderRejectionReason, CompoundBalance[AssetAmount]] = for {
  newFree <- (b.free - assetAmount).toRight(OrderRejectionReason.InsufficientAssetBalance)
} yield CompoundBalance(free = newFree, locked = b.locked + assetAmount)

final case class ClientBalance(
    usdBalance: CompoundBalance[UsdAmount],
    assetBalances: Map[AssetName, CompoundBalance[AssetAmount]]
)

/** Note on scalaz's Dequeue naming:
  *   - snoc(a: A): Dequeue[A] – enqueue on to the back of the queue (similar to `enqueue` in Queue)
  *   - uncons: Maybe[(A, Dequeue[A])] – dequeue from the front of the queue (similar to `dequeueOption` in Queue)
  *
  *   - cons(a: A): Dequeue[A] – enqueue to the front of the queue (when putting partially filled order back) (unique to
  *     Dequeue)
  */
final case class OrderBook(
    buyOrders: TreeMap[AssetPrice, Dequeue[Order]],
    sellOrders: TreeMap[AssetPrice, Dequeue[Order]]
)

object OrderBook {

  def empty: OrderBook = OrderBook(
    buyOrders = TreeMap.empty,
    sellOrders = TreeMap.empty
  )

  /** For newly added order
    */
  def insertBuyOrder(order: Order, book: OrderBook): OrderBook = {
    val newBuyOrders = book.buyOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.snoc(order))
    }
    book.copy(buyOrders = newBuyOrders)
  }

  /** For partially filled order previously taken from the book
    */
  def requeueBuyOrder(order: Order, book: OrderBook): OrderBook = {
    val newBuyOrders = book.buyOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.cons(order))
    }
    book.copy(buyOrders = newBuyOrders)
  }

  /** For newly added order
    */
  def insertSellOrder(order: Order, book: OrderBook): OrderBook = {
    val newSellOrders = book.sellOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.snoc(order))
    }
    book.copy(sellOrders = newSellOrders)
  }

  /** For partially filled order previously taken from the book
    */
  def requeueSellOrder(order: Order, book: OrderBook): OrderBook = {
    val newSellOrders = book.sellOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.cons(order))
    }
    book.copy(sellOrders = newSellOrders)
  }

  /** This function will be called (outside) until the order is fully filled or the queue is empty
    *
    *   - Nothing = there are no orders with given price or better
    *   - Some((order, remainingOrderBook)) = there is an order for given price or better, and the updated orderBook
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  def dequeueMatchingSellOrder(maxPrice: AssetPrice, book: OrderBook): Option[(Order, OrderBook)] =
    book.sellOrders.headOption match {
      case Some((lowestAvailablePrice, orders)) if lowestAvailablePrice <= maxPrice =>
        // Note: price found and matches our requirement
        orders.uncons.toOption match {
          case None =>
            // Note The order queue for given price turned out to be empty.
            // Remove the price with an empty queue from the Map and make a recursive call
            dequeueMatchingSellOrder(maxPrice, book.copy(sellOrders = book.sellOrders - lowestAvailablePrice))
          case Some((order, remainingOrders)) =>
            // There is at least one matching order.
            Some(order, book.copy(sellOrders = book.sellOrders.updated(lowestAvailablePrice, remainingOrders)))
        }
      case _ =>
        // Note: The lowest available price doesn't match our requirement, or the are no more orders.
        // No need to continue searching.
        None
    }

  /** This function will be called (outside) until the order is fully filled or the queue is empty
    *
    *   - Nothing = there are no orders with given price or better
    *   - Some((order, remainingOrderBook)) = there is an order for given price or better, and the updated orderBook
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  def dequeueMatchingBuyOrder(minPrice: AssetPrice, book: OrderBook): Option[(Order, OrderBook)] =
    book.buyOrders.lastOption match {
      case Some((lowestAvailablePrice, orders)) if lowestAvailablePrice >= minPrice =>
        // Note: price found and matches our requirement
        orders.uncons.toOption match {
          case None =>
            // Note The order queue for given price turned out to be empty.
            // Remove the price with an empty queue from the Map and make a recursive call
            dequeueMatchingBuyOrder(minPrice, book.copy(buyOrders = book.buyOrders - lowestAvailablePrice))
          case Some((order, remainingOrders)) =>
            // There is at least one matching order.
            Some(order, book.copy(buyOrders = book.buyOrders.updated(lowestAvailablePrice, remainingOrders)))
        }
      case _ =>
        // Note: The lowest available price doesn't match our requirement, or the are no more orders.
        // No need to continue searching.
        None
    }

}

final case class ExchangeState(
    balances: Map[ClientName, ClientBalance],
    orders: Map[AssetName, OrderBook]
)

object ExchangeState:

  def empty: ExchangeState = ExchangeState(
    balances = Map.empty,
    orders = Map(
      AssetName("A") -> OrderBook.empty,
      AssetName("B") -> OrderBook.empty,
      AssetName("C") -> OrderBook.empty,
      AssetName("D") -> OrderBook.empty
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
        state1 <- lockClientUsd(order.clientName, requiredUsdAmount, state)
      } yield state1
    case OrderSide.Sell =>
      for {
        state1 <- lockClientAsset(order.clientName, order.assetName, order.assetAmount, state)
      } yield state1
  }

  /** TODO: maybe add orderId to lock assets for? */
  private def lockClientUsd(
      clientName: ClientName,
      usdAmount: UsdAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    newUsdBalance <- lockUsd(usdAmount, clientBalance.usdBalance)
    newClientBalance = clientBalance.copy(usdBalance = newUsdBalance)
  } yield state.copy(balances = state.balances.updated(clientName, newClientBalance))

  /** TODO: maybe add orderId to lock assets for? */
  private def lockClientAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance   <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance    <- clientBalance.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newAssetBalance <- lockAsset(assetAmount, assetBalance)
    newClientBalance = clientBalance.copy(assetBalances =
      clientBalance.assetBalances.updated(assetName, newAssetBalance)
    )
  } yield state.copy(balances = state.balances.updated(clientName, newClientBalance))
