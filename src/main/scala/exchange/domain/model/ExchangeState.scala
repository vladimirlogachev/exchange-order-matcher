package exchange.domain.model

import scalaz.Dequeue
import zio.prelude.Equal
import zio.prelude.EqualOps

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.OrderAmounts._
import exchange.domain.model.UsdAmounts._

final case class ExchangeState(
    balances: Map[ClientName, ClientBalance],
    orders: Map[AssetName, OrderBook]
) { self =>

  def clientBalanceTotal: ClientBalanceTotal = {
    def sumAssetAmounts = (assetName: AssetName) =>
      balances.values
        .map(x => x.assetBalances.get(assetName).map(CompoundBalance.totalAssetBalance).getOrElse(AssetAmount.zero))
        .foldLeft(AssetAmount.zero)(_ + _)

    val usd =
      balances.values.map(x => CompoundBalance.totalUsdBalance(x.usdBalance)).foldLeft(UsdAmount.zero)(_ + _)
    val assetA = sumAssetAmounts(AssetName("A"))
    val assetB = sumAssetAmounts(AssetName("B"))
    val assetC = sumAssetAmounts(AssetName("C"))
    val assetD = sumAssetAmounts(AssetName("D"))
    ClientBalanceTotal(usd, assetA, assetB, assetC, assetD)
  }

  def processOrder(order: Order): Either[OrderRejectionReason, ExchangeState] =
    order.side match {
      case OrderSide.Buy =>
        for {
          _      <- checkIfClientHasEnoughFreeUsd(order)
          state1 <- self.buyRecursive(order)
        } yield state1
      case OrderSide.Sell =>
        for {
          _      <- checkIfClientHasEnoughFreeAsset(order)
          state1 <- self.sellRecursive(order)
        } yield state1
    }

  private def checkIfClientHasEnoughFreeUsd(
      order: Order
  ): Either[OrderRejectionReason, Unit] = for {
    usdAmount     <- order.usdAmount.toRight(OrderRejectionReason.UnexpectedInternalError)
    clientBalance <- balances.get(order.clientName).toRight(OrderRejectionReason.ClientNotFound)
    _ <-
      if (clientBalance.usdBalance.free >= usdAmount) then Right(())
      else Left(OrderRejectionReason.InsufficientUsdBalance)
  } yield ()

  private def checkIfClientHasEnoughFreeAsset(
      order: Order
  ): Either[OrderRejectionReason, Unit] = for {
    clientBalance <- balances.get(order.clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance <- clientBalance.assetBalances
      .get(order.assetName)
      .toRight(OrderRejectionReason.UnexpectedInternalError)
    _ <-
      if (assetBalance.free >= order.amount.toAssetAmount) then Right(())
      else Left(OrderRejectionReason.InsufficientAssetBalance)
  } yield ()

  /** This function is called recursively until the order is fully filled or there are no (more) matching orders.
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  private def buyRecursive(buyOrder: Order): Either[OrderRejectionReason, ExchangeState] =
    self.buyStep(buyOrder) match {
      case Right((None, state1))                  => Right(state1)
      case Right((Some(updatedBuyOrder), state1)) => state1.buyRecursive(updatedBuyOrder)
      case Left(rejection)                        => Left(rejection)
    }

  /** This function is called recursively until the order is fully filled or there are no (more) matching orders.
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  private def sellRecursive(sellOrder: Order): Either[OrderRejectionReason, ExchangeState] =
    self.sellStep(sellOrder) match {
      case Right((None, state1))                   => Right(state1)
      case Right((Some(updatedSellOrder), state1)) => state1.sellRecursive(updatedSellOrder)
      case Left(rejection)                         => Left(rejection)
    }

  private def buyStep(buyOrder: Order): Either[OrderRejectionReason, (Option[Order], ExchangeState)] = for {
    maybeOrder <- self.dequeueMatchingSellOrder(buyOrder.assetName, buyOrder.assetPrice)
    res <- maybeOrder match {
      case None =>
        // Note: No (more) matching orders found. Put the (remaining) order in the book.
        self.insertBuyOrder(buyOrder).map((None, _))
      case Some((matchingSellOrder, state1)) => {
        // Note: Matching order found. Process the order.
        state1.buyTrade(buyOrder, matchingSellOrder)
      }
    }
  } yield res

  private def sellStep(sellOrder: Order): Either[OrderRejectionReason, (Option[Order], ExchangeState)] = for {
    maybeOrder <- self.dequeueMatchingBuyOrder(sellOrder.assetName, sellOrder.assetPrice)
    res <- maybeOrder match {
      case None =>
        // Note: No (more) matching orders found. Put the (remaining) order in the book.
        self.insertSellOrder(sellOrder).map((None, _))
      case Some((matchingBuyOrder, state1)) => {
        // Note: Matching order found. Process the order.
        state1.sellTrade(sellOrder, matchingBuyOrder)
      }
    }
  } yield res

  /** Note: Errors are considered unexpected here, because all checks should have already passed before this method.
    */
  private def buyTrade(
      buyOrder: Order,
      matchingSellOrder: Order
  ): Either[OrderRejectionReason, (Option[Order], ExchangeState)] = {
    // Note: The price from the book wins.
    // Note: The price may vary only before the order is put into book.
    val assetName        = buyOrder.assetName
    val tradePrice       = matchingSellOrder.assetPrice
    val tradeAssetAmount = OrderAmount.min(buyOrder.amount, matchingSellOrder.amount).toAssetAmount
    for {
      tradeUsdAmount <- tradeAssetAmount.toUsdAmount(tradePrice).toRight(OrderRejectionReason.UnexpectedInternalError)
      // buyer
      buyer             <- self.balances.get(buyOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      buyerAssetBalance <- buyer.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerUsdFree <- (buyer.usdBalance.free - tradeUsdAmount).toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerAssetFree = buyerAssetBalance.free + tradeAssetAmount
      newBuyerBalance = buyer.copy(
        usdBalance = buyer.usdBalance.copy(free = newBuyerUsdFree),
        assetBalances = buyer.assetBalances.updated(assetName, buyerAssetBalance.copy(free = newBuyerAssetFree))
      )
      state1 = self.copy(balances = self.balances.updated(buyOrder.clientName, newBuyerBalance))

      // seller
      seller <- state1.balances.get(matchingSellOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      sellerAssetBalance <- seller.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerUsdFree = (seller.usdBalance.free + tradeUsdAmount)
      newSellerAssetFree <- (sellerAssetBalance.free - tradeAssetAmount)
        .toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerBalance = seller.copy(
        usdBalance = seller.usdBalance.copy(free = newSellerUsdFree),
        assetBalances = seller.assetBalances.updated(assetName, sellerAssetBalance.copy(free = newSellerAssetFree))
      )
      state2 = state1.copy(balances = state1.balances.updated(matchingSellOrder.clientName, newSellerBalance))
      res <- {
        if (buyOrder.amount === matchingSellOrder.amount) then {
          // Note: The order is fully filled by another matching order
          // No orders are going into the book.
          Right(None, state2)
        } else if (buyOrder.amount.toAssetAmount < matchingSellOrder.amount.toAssetAmount) then {
          // Note: The order is fully filled by another matching order,
          // and we need to put the remainings of the matching order back into the book.
          for {
            updatedMatchingAmount <- (matchingSellOrder.amount.toAssetAmount - tradeAssetAmount)
              .flatMap(OrderAmount.fromAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedMatchingOrder = matchingSellOrder.copy(amount = updatedMatchingAmount)
            orderBookForAsset <- state2.orders
              .get(buyOrder.assetName)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            state3 <- state2.requeueSellOrder(updatedMatchingOrder)
          } yield (None, state3)
        } else {
          // Note: The order is partially filled by another existing order, and we need to make a recursive call.
          for {
            updatedOrderAmount <- (buyOrder.amount.toAssetAmount - tradeAssetAmount)
              .flatMap(OrderAmount.fromAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedOrder = buyOrder.copy(amount = updatedOrderAmount)
          } yield (Some(updatedOrder), state2)
        }
      }
    } yield res
  }

  /** Note: Errors are considered unexpected here, because all checks should have already passed before this method.
    */
  private def sellTrade(
      sellOrder: Order,
      matchingBuyOrder: Order
  ): Either[OrderRejectionReason, (Option[Order], ExchangeState)] = {
    // Note: The price from the book wins.
    // Note: The price may vary only before the order is put into book.
    val assetName        = sellOrder.assetName
    val tradePrice       = matchingBuyOrder.assetPrice
    val tradeAssetAmount = OrderAmount.min(sellOrder.amount, matchingBuyOrder.amount).toAssetAmount
    for {
      tradeUsdAmount <- tradeAssetAmount.toUsdAmount(tradePrice).toRight(OrderRejectionReason.UnexpectedInternalError)
      // buyer
      buyer             <- self.balances.get(matchingBuyOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      buyerAssetBalance <- buyer.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerUsdFree <- (buyer.usdBalance.free - tradeUsdAmount)
        .toRight(OrderRejectionReason.UnexpectedInternalError)
      newBuyerAssetFree = buyerAssetBalance.free + tradeAssetAmount
      newBuyerBalance = buyer.copy(
        usdBalance = buyer.usdBalance.copy(free = newBuyerUsdFree),
        assetBalances = buyer.assetBalances.updated(assetName, buyerAssetBalance.copy(free = newBuyerAssetFree))
      )
      state1 = self.copy(balances = self.balances.updated(matchingBuyOrder.clientName, newBuyerBalance))
      // seller
      seller             <- state1.balances.get(sellOrder.clientName).toRight(OrderRejectionReason.ClientNotFound)
      sellerAssetBalance <- seller.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerUsdFree = (seller.usdBalance.free + tradeUsdAmount)
      newSellerAssetFree <- (sellerAssetBalance.free - tradeAssetAmount)
        .toRight(OrderRejectionReason.UnexpectedInternalError)
      newSellerBalance = seller.copy(
        usdBalance = seller.usdBalance.copy(free = newSellerUsdFree),
        assetBalances = seller.assetBalances.updated(assetName, sellerAssetBalance.copy(free = newSellerAssetFree))
      )
      state2 = state1.copy(balances = state1.balances.updated(sellOrder.clientName, newSellerBalance))
      res <- {
        if (sellOrder.amount === matchingBuyOrder.amount) then {
          // Note: The order is fully filled by another matching order
          // No orders are going into the book.
          Right(None, state2)
        } else if (sellOrder.amount.toAssetAmount < matchingBuyOrder.amount.toAssetAmount) then {
          // Note: The order is fully filled by another matching order,
          // and we need to put the remainings of the matching order back into the book.
          for {
            updatedMatchingAmount <- (matchingBuyOrder.amount.toAssetAmount - tradeAssetAmount)
              .flatMap(OrderAmount.fromAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedMatchingOrder = matchingBuyOrder.copy(amount = updatedMatchingAmount)
            orderBookForAsset <- state2.orders
              .get(sellOrder.assetName)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            state3 <- state2.requeueBuyOrder(updatedMatchingOrder)
          } yield (None, state3)
        } else {
          // Note: The order is partially filled by another existing order, and we need to make a recursive call.
          for {
            updatedOrderAmount <- (sellOrder.amount.toAssetAmount - tradeAssetAmount)
              .flatMap(OrderAmount.fromAssetAmount)
              .toRight(OrderRejectionReason.UnexpectedInternalError)
            updatedOrder = sellOrder.copy(amount = updatedOrderAmount)
          } yield (Some(updatedOrder), state2)
        }
      }
    } yield res
  }

  /* ---------------- Functions that maintain consistency between OrderBook and locked assets ---------------- */

  /** This function will be called (outside) until the order is fully filled or the queue is empty
    *
    *   - Nothing = there are no orders with given price or better
    *   - Some((order, remainingOrderBook)) = there is an order for given price or better, and the updated orderBook
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  private def dequeueMatchingSellOrder(
      assetName: AssetName,
      maxPrice: AssetPrice
  ): Either[OrderRejectionReason, Option[(Order, ExchangeState)]] =
    orders.get(assetName) match {
      case None => Left(OrderRejectionReason.UnexpectedInternalError)
      case Some(book) =>
        book.sellOrders.headOption match {
          case Some((lowestAvailablePrice, queue)) if lowestAvailablePrice <= maxPrice =>
            // Note: price found and matches our requirement
            queue.uncons.toOption match {
              case None =>
                // Note The order queue for given price turned out to be empty.
                // Remove the price with an empty queue from the Map and make a recursive call
                val updatedBook = book.copy(sellOrders = book.sellOrders - lowestAvailablePrice)
                val state1      = self.copy(orders = orders.updated(assetName, updatedBook))
                state1.dequeueMatchingSellOrder(assetName, maxPrice)
              case Some((order, remainingOrders)) =>
                // There is at least one matching order.
                val updatedBook = book.copy(sellOrders = book.sellOrders.updated(lowestAvailablePrice, remainingOrders))
                val state1      = self.copy(orders = orders.updated(assetName, updatedBook))
                for {
                  state2 <- state1.unlockClientAsset(order.clientName, assetName, order.amount.toAssetAmount)
                } yield Some(order, state2)
            }
          case _ =>
            // Note: The lowest available price doesn't match our requirement, or the are no more orders.
            // No need to continue searching.
            Right(None)
        }
    }

  /** This function will be called (outside) until the order is fully filled or the queue is empty
    *
    *   - Nothing = there are no orders with given price or better
    *   - Some((order, remainingOrderBook)) = there is an order for given price or better, and the updated orderBook
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  private def dequeueMatchingBuyOrder(
      assetName: AssetName,
      minPrice: AssetPrice
  ): Either[OrderRejectionReason, Option[(Order, ExchangeState)]] =
    orders.get(assetName) match {
      case None => Left(OrderRejectionReason.UnexpectedInternalError)
      case Some(book) =>
        book.buyOrders.lastOption match {
          case Some((lowestAvailablePrice, queue)) if lowestAvailablePrice >= minPrice =>
            // Note: price found and matches our requirement
            queue.uncons.toOption match {
              case None =>
                // Note The order queue for given price turned out to be empty.
                // Remove the price with an empty queue from the Map and make a recursive call
                val updatedBook = book.copy(buyOrders = book.buyOrders - lowestAvailablePrice)
                val state1      = self.copy(orders = orders.updated(assetName, updatedBook))
                state1.dequeueMatchingBuyOrder(assetName, minPrice)
              case Some((order, remainingOrders)) =>
                // There is at least one matching order.
                val updatedBook = book.copy(buyOrders = book.buyOrders.updated(lowestAvailablePrice, remainingOrders))
                val state1      = self.copy(orders = orders.updated(assetName, updatedBook))
                for {
                  usdAmount <- order.usdAmount.toRight(OrderRejectionReason.UnexpectedInternalError)
                  state2    <- state1.unlockClientUsd(order.clientName, usdAmount)
                } yield Some(order, state2)
            }
          case _ =>
            // Note: The lowest available price doesn't match our requirement, or the are no more orders.
            // No need to continue searching.
            Right(None)
        }
    }

  /** For newly added order. Locks usd.
    */
  private def insertBuyOrder(order: Order): Either[OrderRejectionReason, ExchangeState] = for {
    usdAmount <- order.usdAmount.toRight(OrderRejectionReason.UnexpectedInternalError)
    state1    <- self.lockClientUsd(order.clientName, usdAmount)
    book      <- orders.get(order.assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newBuyOrders = book.buyOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.snoc(order))
    }
  } yield state1.copy(orders = orders.updated(order.assetName, book.copy(buyOrders = newBuyOrders)))

  /** For newly added order. Locks assets.
    */
  private def insertSellOrder(order: Order): Either[OrderRejectionReason, ExchangeState] = for {
    state1 <- self.lockClientAsset(order.clientName, order.assetName, order.amount.toAssetAmount)
    book   <- orders.get(order.assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newSellOrders = book.sellOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.snoc(order))
    }
  } yield state1.copy(orders = orders.updated(order.assetName, book.copy(sellOrders = newSellOrders)))

  /** For partially filled order previously taken from the book
    */
  private def requeueBuyOrder(order: Order): Either[OrderRejectionReason, ExchangeState] = for {
    usdAmount <- order.usdAmount.toRight(OrderRejectionReason.UnexpectedInternalError)
    state1    <- self.lockClientUsd(order.clientName, usdAmount)
    book      <- orders.get(order.assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newBuyOrders = book.buyOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.cons(order))
    }
  } yield state1.copy(orders = orders.updated(order.assetName, book.copy(buyOrders = newBuyOrders)))

  /** For partially filled order previously taken from the book
    */
  private def requeueSellOrder(order: Order): Either[OrderRejectionReason, ExchangeState] = for {
    state1 <- self.lockClientAsset(order.clientName, order.assetName, order.amount.toAssetAmount)
    book   <- orders.get(order.assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newSellOrders = book.sellOrders.updatedWith(order.assetPrice) {
      case None         => Some(Dequeue(order))
      case Some(orders) => Some(orders.cons(order))
    }
  } yield state1.copy(orders = orders.updated(order.assetName, book.copy(sellOrders = newSellOrders)))

  /* ---------------- Helper functions (to use only when moving orders in the OrderBook) ---------------- */

  private def lockClientUsd(
      clientName: ClientName,
      usdAmount: UsdAmount
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    newUsdFree    <- (clientBalance.usdBalance.free - usdAmount).toRight(OrderRejectionReason.InsufficientUsdBalance)
    newUsdBalance    = CompoundBalance(free = newUsdFree, locked = clientBalance.usdBalance.locked + usdAmount)
    newClientBalance = clientBalance.copy(usdBalance = newUsdBalance)
  } yield self.copy(balances = balances.updated(clientName, newClientBalance))

  private def unlockClientUsd(
      clientName: ClientName,
      usdAmount: UsdAmount
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    newUsdLocked  <- (clientBalance.usdBalance.locked - usdAmount).toRight(OrderRejectionReason.UnexpectedInternalError)
    newUsdBalance    = CompoundBalance(free = clientBalance.usdBalance.free + usdAmount, locked = newUsdLocked)
    newClientBalance = clientBalance.copy(usdBalance = newUsdBalance)
  } yield self.copy(balances = balances.updated(clientName, newClientBalance))

  private def lockClientAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance  <- clientBalance.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newAssetFree  <- (assetBalance.free - assetAmount).toRight(OrderRejectionReason.InsufficientAssetBalance)
    newAssetBalance = CompoundBalance(free = newAssetFree, locked = assetBalance.locked + assetAmount)
    newClientBalance = clientBalance.copy(assetBalances =
      clientBalance.assetBalances.updated(assetName, newAssetBalance)
    )
  } yield self.copy(balances = balances.updated(clientName, newClientBalance))

  private def unlockClientAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance  <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance   <- clientBalance.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    newAssetLocked <- (assetBalance.locked - assetAmount).toRight(OrderRejectionReason.UnexpectedInternalError)
    newAssetBalance = CompoundBalance(free = assetBalance.free + assetAmount, locked = newAssetLocked)
    newClientBalance = clientBalance.copy(assetBalances =
      clientBalance.assetBalances.updated(assetName, newAssetBalance)
    )
  } yield self.copy(balances = balances.updated(clientName, newClientBalance))

}

object ExchangeState {
  def empty: ExchangeState = ExchangeState(
    balances = Map.empty,
    orders = Map(
      AssetName("A") -> OrderBook.empty,
      AssetName("B") -> OrderBook.empty,
      AssetName("C") -> OrderBook.empty,
      AssetName("D") -> OrderBook.empty
    )
  )

}

final case class ClientBalanceTotal(
    usd: UsdAmount,
    assetA: AssetAmount,
    assetB: AssetAmount,
    assetC: AssetAmount,
    assetD: AssetAmount
) {
  override def toString(): String = s"Usd: ${usd}, A: ${assetA}, B: ${assetB}, C: ${assetC}, D: ${assetD}"
}

object ClientBalanceTotal:
  implicit val ClientBalanceTotalEqual: Equal[ClientBalanceTotal] =
    Equal.default
