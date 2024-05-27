package exchange.domain.model

import scala.collection.immutable.TreeMap

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import scalaz.Dequeue
import zio.prelude.Equal
import zio.prelude.EqualOps

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

def totalAssetBalance(x: CompoundBalance[AssetAmount]) = x.free + x.locked

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

private object OrderBook {

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

object Exchange {

  def processOrder(
      order: Order,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = order.side match {
    case OrderSide.Buy =>
      for {
        requiredUsdAmount <- order.assetAmount
          .toUsdAmount(order.assetPrice)
          .toRight(OrderRejectionReason.UnexpectedInternalError)
        _      <- checkIfClientHasEnoughFreeUsd(order.clientName, requiredUsdAmount, state)
        state1 <- recursiveBuy(order, state)
      } yield state1
    case OrderSide.Sell =>
      for {
        _      <- checkIfClientHasEnoughFreeAsset(order.clientName, order.assetName, order.assetAmount, state)
        state1 <- recursiveSell(order, state)
      } yield state1
  }

  /** This function is called recursively until the order is fully filled or there are no (more) matching orders.
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  def recursiveBuy(buyOrder: Order, state: ExchangeState): Either[OrderRejectionReason, ExchangeState] =
    state.orders.get(buyOrder.assetName) match {
      case None => Left(OrderRejectionReason.UnexpectedInternalError)
      case Some(book) =>
        OrderBook.dequeueMatchingSellOrder(buyOrder.assetPrice, book) match {
          case None =>
            for {
              // Note: No (more) matching orders found. Put the (remaining) order in the book.
              requiredUsdAmount <- buyOrder.assetAmount
                .toUsdAmount(buyOrder.assetPrice)
                .toRight(OrderRejectionReason.UnexpectedInternalError)
              state1 <- lockClientUsd(buyOrder.clientName, requiredUsdAmount, state)
              state2 = state1.copy(
                orders = state1.orders.updated(buyOrder.assetName, OrderBook.insertBuyOrder(buyOrder, book))
              )
            } yield state2
          case Some((matchingSellOrder, remainingBook)) => {
            // Note: Matching order found. Process the order.
            val state1 = state.copy(orders = state.orders.updated(buyOrder.assetName, remainingBook))
            val res    = tradeBuyFromFreeSellFromLocked(buyOrder, matchingSellOrder, state1)
            res match {
              case Right((None, state2))                  => Right(state2)
              case Right((Some(updatedBuyOrder), state2)) => recursiveBuy(updatedBuyOrder, state2)
              case Left(rejection)                        => Left(rejection)
            }
          }
        }
    }

  /** This function is called recursively until the order is fully filled or there are no (more) matching orders.
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  def recursiveSell(sellOrder: Order, state: ExchangeState): Either[OrderRejectionReason, ExchangeState] =
    state.orders.get(sellOrder.assetName) match {
      case None => Left(OrderRejectionReason.UnexpectedInternalError)
      case Some(book) =>
        OrderBook.dequeueMatchingBuyOrder(sellOrder.assetPrice, book) match {
          case None =>
            for {
              // Note: No (more) matching orders found. Put the (remaining) order in the book.
              state1 <- lockClientAsset(sellOrder.clientName, sellOrder.assetName, sellOrder.assetAmount, state)
              state2 = state1.copy(
                orders = state1.orders.updated(sellOrder.assetName, OrderBook.insertSellOrder(sellOrder, book))
              )
            } yield state2
          case Some((matchingBuyOrder, remainingBook)) => {
            // Note: Matching order found. Process the order.
            val state1 = state.copy(orders = state.orders.updated(sellOrder.assetName, remainingBook))
            val res    = tradeBuyFromLockedSellFromFree(sellOrder, matchingBuyOrder, state1)
            res match {
              case Right((None, state2))                   => Right(state2)
              case Right((Some(updatedSellOrder), state2)) => recursiveSell(updatedSellOrder, state2)
              case Left(rejection)                         => Left(rejection)
            }
          }
        }
    }

  /** Note: Errors are considered unexpected here, because all checks should have already passed before this method.
    */
  def tradeBuyFromFreeSellFromLocked(
      buyOrder: Order,
      matchingSellOrder: Order,
      state: ExchangeState
  ): Either[OrderRejectionReason, (Option[Order], ExchangeState)] = {
    // Note: The price from the book wins. Locked assets will always be enough and there will be no leftovers.
    // Note: The price may vary only before the order is put into book.
    val assetName        = buyOrder.assetName
    val tradePrice       = matchingSellOrder.assetPrice
    val tradeAssetAmount = AssetAmount.min(buyOrder.assetAmount, matchingSellOrder.assetAmount)
    for {
      tradeUsdAmount <- tradeAssetAmount.toUsdAmount(tradePrice).toRight(OrderRejectionReason.UnexpectedInternalError)
      // buyer
      buyer             <- state.balances.get(buyOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      buyerAssetBalance <- buyer.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerUsdFree <- (buyer.usdBalance.free - tradeUsdAmount).toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerAssetFree = buyerAssetBalance.free + tradeAssetAmount
      newBuyerBalance = buyer.copy(
        usdBalance = buyer.usdBalance.copy(free = newBuyerUsdFree),
        assetBalances = buyer.assetBalances.updated(assetName, buyerAssetBalance.copy(free = newBuyerAssetFree))
      )
      // seller
      seller <- state.balances.get(matchingSellOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      sellerAssetBalance <- seller.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerUsdFree = (seller.usdBalance.free + tradeUsdAmount)
      newSellerAssetLocked <- (sellerAssetBalance.locked - tradeAssetAmount)
        .toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerBalance = seller.copy(
        usdBalance = seller.usdBalance.copy(free = newSellerUsdFree),
        assetBalances = seller.assetBalances.updated(assetName, sellerAssetBalance.copy(locked = newSellerAssetLocked))
      )
      state1 = state.copy(
        balances = state.balances
          .updated(buyOrder.clientName, newBuyerBalance)
          .updated(matchingSellOrder.clientName, newSellerBalance)
      )
      out <- {
        if (buyOrder.assetAmount === matchingSellOrder.assetAmount) then {
          // Note: The order is fully filled by another matching order
          // No orders are going into the book.
          Right(None, state1)
        } else if (buyOrder.assetAmount < matchingSellOrder.assetAmount) then {
          // Note: The order is fully filled by another matching order,
          // and we need to put the remainings of the matching order back into the book.
          for {
            updatedMatchingAmount <- (matchingSellOrder.assetAmount - tradeAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedMatchingOrder = matchingSellOrder.copy(assetAmount = updatedMatchingAmount)
            orderBookForAsset <- state1.orders
              .get(buyOrder.assetName)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            state2 = state1.copy(
              orders = state1.orders
                .updated(assetName, OrderBook.requeueSellOrder(updatedMatchingOrder, orderBookForAsset))
            )
          } yield (None, state2)
        } else {
          // Note: The order is partially filled by another existing order, and we need to make a recursive call.
          for {
            updatedOrderAmount <- (buyOrder.assetAmount - tradeAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedOrder = buyOrder.copy(assetAmount = updatedOrderAmount)
          } yield (Some(updatedOrder), state1)
        }
      }
    } yield out
  }

  /** Note: Errors are considered unexpected here, because all checks should have already passed before this method.
    */
  def tradeBuyFromLockedSellFromFree(
      sellOrder: Order,
      matchingBuyOrder: Order,
      state: ExchangeState
  ): Either[OrderRejectionReason, (Option[Order], ExchangeState)] = {
    // Note: The price from the book wins. Locked assets will always be enough and there will be no leftovers.
    // Note: The price may vary only before the order is put into book.
    val assetName        = sellOrder.assetName
    val tradePrice       = matchingBuyOrder.assetPrice
    val tradeAssetAmount = AssetAmount.min(sellOrder.assetAmount, matchingBuyOrder.assetAmount)
    for {
      tradeUsdAmount <- tradeAssetAmount.toUsdAmount(tradePrice).toRight(OrderRejectionReason.UnexpectedInternalError)
      // buyer
      buyer             <- state.balances.get(matchingBuyOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      buyerAssetBalance <- buyer.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerUsdLocked <- (buyer.usdBalance.locked - tradeUsdAmount)
        .toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerAssetFree = buyerAssetBalance.free + tradeAssetAmount
      newBuyerBalance = buyer.copy(
        usdBalance = buyer.usdBalance.copy(locked = newBuyerUsdLocked),
        assetBalances = buyer.assetBalances.updated(assetName, buyerAssetBalance.copy(free = newBuyerAssetFree))
      )
      // seller
      seller             <- state.balances.get(sellOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      sellerAssetBalance <- seller.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerUsdFree = (seller.usdBalance.free + tradeUsdAmount)
      newSellerAssetFree <- (sellerAssetBalance.free - tradeAssetAmount)
        .toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerBalance = seller.copy(
        usdBalance = seller.usdBalance.copy(free = newSellerUsdFree),
        assetBalances = seller.assetBalances.updated(assetName, sellerAssetBalance.copy(free = newSellerAssetFree))
      )
      state1 = state.copy(
        balances = state.balances
          .updated(matchingBuyOrder.clientName, newBuyerBalance)
          .updated(sellOrder.clientName, newSellerBalance)
      )
      out <- {
        if (sellOrder.assetAmount === matchingBuyOrder.assetAmount) then {
          // Note: The order is fully filled by another matching order
          // No orders are going into the book.
          Right(None, state1)
        } else if (sellOrder.assetAmount < matchingBuyOrder.assetAmount) then {
          // Note: The order is fully filled by another matching order,
          // and we need to put the remainings of the matching order back into the book.
          for {
            updatedMatchingAmount <- (matchingBuyOrder.assetAmount - tradeAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedMatchingOrder = matchingBuyOrder.copy(assetAmount = updatedMatchingAmount)
            orderBookForAsset <- state1.orders
              .get(sellOrder.assetName)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            state2 = state1.copy(
              orders = state1.orders
                .updated(assetName, OrderBook.requeueBuyOrder(updatedMatchingOrder, orderBookForAsset))
            )
          } yield (None, state2)
        } else {
          // Note: The order is partially filled by another existing order, and we need to make a recursive call.
          for {
            updatedOrderAmount <- (sellOrder.assetAmount - tradeAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedOrder = sellOrder.copy(assetAmount = updatedOrderAmount)
          } yield (Some(updatedOrder), state1)
        }
      }
    } yield out
  }

  private def checkIfClientHasEnoughFreeUsd(
      clientName: ClientName,
      usdAmount: UsdAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, Unit] = for {
    clientBalance <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    _ <-
      if (clientBalance.usdBalance.free >= usdAmount) then Right(())
      else Left(OrderRejectionReason.InsufficientUsdBalance)
  } yield ()

  private def checkIfClientHasEnoughFreeAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, Unit] = for {
    clientBalance <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance  <- clientBalance.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    _ <-
      if (assetBalance.free >= assetAmount) then Right(())
      else Left(OrderRejectionReason.InsufficientAssetBalance)
  } yield ()

  private def lockClientUsd(
      clientName: ClientName,
      usdAmount: UsdAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    newUsdFree    <- (clientBalance.usdBalance.free - usdAmount).toRight(OrderRejectionReason.InsufficientUsdBalance)
    newUsdBalance    = CompoundBalance(free = newUsdFree, locked = clientBalance.usdBalance.locked + usdAmount)
    newClientBalance = clientBalance.copy(usdBalance = newUsdBalance)
  } yield state.copy(balances = state.balances.updated(clientName, newClientBalance))

  private def lockClientAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount,
      state: ExchangeState
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- state.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance  <- clientBalance.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newAssetFree  <- (assetBalance.free - assetAmount).toRight(OrderRejectionReason.InsufficientAssetBalance)
    newAssetBalance = CompoundBalance(free = newAssetFree, locked = assetBalance.locked + assetAmount)
    newClientBalance = clientBalance.copy(assetBalances =
      clientBalance.assetBalances.updated(assetName, newAssetBalance)
    )
  } yield state.copy(balances = state.balances.updated(clientName, newClientBalance))

}
