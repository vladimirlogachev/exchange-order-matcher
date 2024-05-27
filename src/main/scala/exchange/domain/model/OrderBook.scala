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

private object OrderBook:
  def empty: OrderBook = OrderBook(
    buyOrders = TreeMap.empty,
    sellOrders = TreeMap.empty
  )
