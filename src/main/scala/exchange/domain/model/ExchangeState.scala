package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import zio.prelude.Equal
import zio.prelude.EqualOps

enum OrderRejectionReason:
  case ClientNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance
  case InvalidAssetAmount
  case UnexpectedInternalError // TODO: consider removing or defining more specific errors

object OrderRejectionReason:
  implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
    Equal.default

final case class CompoundBalance[A](
    free: A,
    locked: A
)

object CompoundBalance:
  def totalUsdBalance(x: CompoundBalance[UsdAmount]) = x.free + x.locked

  def totalAssetBalance(x: CompoundBalance[AssetAmount]) = x.free + x.locked

final case class ClientBalance(
    usdBalance: CompoundBalance[UsdAmount],
    assetBalances: Map[AssetName, CompoundBalance[AssetAmount]]
)

final case class ExchangeState(
    balances: Map[ClientName, ClientBalance],
    orders: Map[AssetName, OrderBook]
) { self =>
  def processOrder(order: Order): Either[OrderRejectionReason, ExchangeState] =
    order.side match {
      case OrderSide.Buy =>
        for {
          _ <- checkIfOrderHasNonZeroAmount(order)
          requiredUsdAmount <- order.assetAmount
            .toUsdAmount(order.assetPrice)
            .toRight(OrderRejectionReason.UnexpectedInternalError)
          _      <- checkIfClientHasEnoughFreeUsd(order.clientName, requiredUsdAmount)
          state1 <- recursiveBuy(order)
        } yield state1
      case OrderSide.Sell =>
        for {
          _      <- checkIfOrderHasNonZeroAmount(order)
          _      <- checkIfClientHasEnoughFreeAsset(order.clientName, order.assetName, order.assetAmount)
          state1 <- recursiveSell(order)
        } yield state1
    }

  private def checkIfClientHasEnoughFreeUsd(
      clientName: ClientName,
      usdAmount: UsdAmount
  ): Either[OrderRejectionReason, Unit] = for {
    clientBalance <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    _ <-
      if (clientBalance.usdBalance.free >= usdAmount) then Right(())
      else Left(OrderRejectionReason.InsufficientUsdBalance)
  } yield ()

  private def checkIfClientHasEnoughFreeAsset(
      clientName: ClientName,
      assetName: AssetName,
      assetAmount: AssetAmount
  ): Either[OrderRejectionReason, Unit] = for {
    clientBalance <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    assetBalance  <- clientBalance.assetBalances.get(assetName).toRight(OrderRejectionReason.UnexpectedInternalError)
    _ <-
      if (assetBalance.free >= assetAmount) then Right(())
      else Left(OrderRejectionReason.InsufficientAssetBalance)
  } yield ()

  /** This function is called recursively until the order is fully filled or there are no (more) matching orders.
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  private def recursiveBuy(buyOrder: Order): Either[OrderRejectionReason, ExchangeState] =
    buyStep(buyOrder) match {
      case Right((None, state))                  => Right(state)
      case Right((Some(updatedBuyOrder), state)) => state.recursiveBuy(updatedBuyOrder)
      case Left(rejection)                       => Left(rejection)
    }

  private def buyStep(buyOrder: Order): Either[OrderRejectionReason, (Option[Order], ExchangeState)] =
    orders.get(buyOrder.assetName) match {
      case None => Left(OrderRejectionReason.UnexpectedInternalError)
      case Some(book) =>
        OrderBook.dequeueMatchingSellOrder(buyOrder.assetPrice, book) match {
          case None =>
            for {
              // Note: No (more) matching orders found. Put the (remaining) order in the book.
              requiredUsdAmount <- buyOrder.assetAmount
                .toUsdAmount(buyOrder.assetPrice)
                .toRight(OrderRejectionReason.UnexpectedInternalError)
              state1 <- lockClientUsd(buyOrder.clientName, requiredUsdAmount)
              state2 = state1.copy(
                orders = state1.orders.updated(buyOrder.assetName, OrderBook.insertBuyOrder(buyOrder, book))
              )
            } yield (None, state2)
          case Some((matchingSellOrder, remainingBook)) => {
            // Note: Matching order found. Process the order.
            val state1 = self.copy(orders = orders.updated(buyOrder.assetName, remainingBook))
            tradeBuyFromFreeSellFromLocked(buyOrder, matchingSellOrder, state1)
          }
        }
    }

  /** This function is called recursively until the order is fully filled or there are no (more) matching orders.
    *
    * Note: this function uses pattern matching insead of for comprehension to allow for a tail call.
    */
  @annotation.tailrec
  private def recursiveSell(sellOrder: Order): Either[OrderRejectionReason, ExchangeState] =
    sellStep(sellOrder) match {
      case Right((None, state))                   => Right(state)
      case Right((Some(updatedSellOrder), state)) => state.recursiveSell(updatedSellOrder)
      case Left(rejection)                        => Left(rejection)
    }

  private def sellStep(sellOrder: Order): Either[OrderRejectionReason, (Option[Order], ExchangeState)] =
    orders.get(sellOrder.assetName) match {
      case None => Left(OrderRejectionReason.UnexpectedInternalError)
      case Some(book) =>
        OrderBook.dequeueMatchingBuyOrder(sellOrder.assetPrice, book) match {
          case None =>
            for {
              // Note: No (more) matching orders found. Put the (remaining) order in the book.
              state1 <- lockClientAsset(sellOrder.clientName, sellOrder.assetName, sellOrder.assetAmount)
              state2 = state1.copy(
                orders = state1.orders.updated(sellOrder.assetName, OrderBook.insertSellOrder(sellOrder, book))
              )
            } yield (None, state2)
          case Some((matchingBuyOrder, remainingBook)) => {
            // Note: Matching order found. Process the order.
            val state1 = self.copy(orders = orders.updated(sellOrder.assetName, remainingBook))
            tradeBuyFromLockedSellFromFree(sellOrder, matchingBuyOrder, state1)
          }
        }
    }

  /** Note: Errors are considered unexpected here, because all checks should have already passed before this method.
    */
  private def tradeBuyFromFreeSellFromLocked(
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
  private def tradeBuyFromLockedSellFromFree(
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

  private def lockClientUsd(
      clientName: ClientName,
      usdAmount: UsdAmount
  ): Either[OrderRejectionReason, ExchangeState] = for {
    clientBalance <- balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
    newUsdFree    <- (clientBalance.usdBalance.free - usdAmount).toRight(OrderRejectionReason.InsufficientUsdBalance)
    newUsdBalance    = CompoundBalance(free = newUsdFree, locked = clientBalance.usdBalance.locked + usdAmount)
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

// TODO: move
private def checkIfOrderHasNonZeroAmount(order: Order): Either[OrderRejectionReason, Unit] =
  if order.assetAmount === AssetAmount.zero
  then Left(OrderRejectionReason.InvalidAssetAmount)
  else Right(())
