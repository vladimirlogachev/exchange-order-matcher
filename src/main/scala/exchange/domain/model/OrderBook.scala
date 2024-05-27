package exchange.domain.model

import scala.collection.immutable.TreeMap

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetPrices._
import scalaz.Dequeue

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
