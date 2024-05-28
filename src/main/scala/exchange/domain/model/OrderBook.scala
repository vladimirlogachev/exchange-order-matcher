package exchange.domain.model

import scala.collection.immutable.TreeMap

import scalaz.Dequeue

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetPrices._

/** OrderBook is the storage for buy and sell orders, ordered by price and then by the order of placement.
  *
  *   - TreeMap is used to keep the orders ordered by price
  *     - Take or update the lowest or the highest price = O(log n)
  *     - Insert an arbitrary price = O(log n)
  *   - Dequeue is used to keep the orders in the order they were added
  *     - dequeue from the front = O(1)
  *     - enque to the back = O(1)
  *     - enque in front = O(1)
  *
  * Note on scalaz's Dequeue naming:
  *   - snoc(a: A): Dequeue[A] – enqueue on to the back of the queue (similar to `enqueue` in Queue)
  *   - uncons: Maybe[(A, Dequeue[A])] – dequeue from the front of the queue (similar to `dequeueOption` in Queue)
  *
  *   - cons(a: A): Dequeue[A] – enqueue to the front of the queue (when putting partially filled order back) (unique to
  *     Dequeue)
  */
private final case class OrderBook(
    buyOrders: TreeMap[AssetPrice, Dequeue[Order]],
    sellOrders: TreeMap[AssetPrice, Dequeue[Order]]
)

private object OrderBook:
  def empty: OrderBook = OrderBook(
    buyOrders = TreeMap.empty,
    sellOrders = TreeMap.empty
  )
