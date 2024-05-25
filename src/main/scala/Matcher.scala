import AssetAmount._
import AssetName._
import AssetPrice._
import ClientName._
import UsdAmount._
import zio._
import zio.prelude._
import zio.stream._

final case class ClientBalances(
    usdBalance: UsdAmount,
    assetBalances: Map[AssetName, AssetAmount]
)

final case class MatcherState(
    balances: Map[ClientName, ClientBalances], // TODO: locked assets and usd
    pendingOrders: List[ClientOrder]           // TODO: not a list
)

object MatcherState:
  def empty: MatcherState = MatcherState(balances = Map.empty, pendingOrders = List.empty)

def toFinalBalances(state: MatcherState): Seq[ClientBalanceRecord] = state.balances.map {
  case (clientName, clientBalances) =>
    ClientBalanceRecord(
      clientName,
      clientBalances.usdBalance,
      clientBalances.assetBalances(AssetName("A")),
      clientBalances.assetBalances(AssetName("B")),
      clientBalances.assetBalances(AssetName("C")),
      clientBalances.assetBalances(AssetName("D"))
    )
}.toSeq

enum ClientLoadError:
  case ClientAlreadyExists

implicit val ClientLoadErrorEqual: Equal[ClientLoadError] =
  Equal.default

def loadClientBalance(
    record: ClientBalanceRecord,
    state: MatcherState
): Either[ClientLoadError, MatcherState] =
  if state.balances.contains(record.clientName) then Left(ClientLoadError.ClientAlreadyExists)
  else
    val clientBalances = Map(
      AssetName("A") -> record.balanceA,
      AssetName("B") -> record.balanceB,
      AssetName("C") -> record.balanceC,
      AssetName("D") -> record.balanceD
    )
    val allBalances = state.balances.updated(record.clientName, ClientBalances(record.usdBalance, clientBalances))
    Right(state.copy(balances = allBalances))

enum OrderRejectionReason:
  case ClientNotFound
  // TODO: try to get rid of it, e.g.:
  // InsufficientAssetBalance when failed to sell
  // create not found for buying
  case AssetBalanceNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance

def processOrder(
    clientOrder: ClientOrder,
    state: MatcherState
): Either[OrderRejectionReason, MatcherState] = clientOrder match {
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

enum MatcherError:
  case ItsInputStreamError(e: Throwable)
  case ItsClientLoadError(e: ClientLoadError)

implicit val MatcherErrorEqual: Equal[MatcherError] =
  Equal.default

final case class MatcherOutput(
    state: MatcherState,
    rejectedOrders: List[(ClientOrder, OrderRejectionReason)]
)

object MatcherOutput:

  implicit val MatcherOutputEqual: Equal[MatcherOutput] =
    Equal.default

def runMatcher(
    clientBalances: ZStream[Any, Throwable, ClientBalanceRecord],
    orders: ZStream[Any, Throwable, ClientOrder]
): IO[MatcherError, MatcherOutput] = {

  for {
    model1 <- clientBalances
      .run(
        ZSink.foldLeft(Right(MatcherState.empty): Either[ClientLoadError, MatcherState])((state, clientBalance) =>
          state.flatMap(loadClientBalance(clientBalance, _))
        )
      )
      .mapError(MatcherError.ItsInputStreamError(_))
    model2 <- ZIO
      .fromEither(model1)
      .mapError(MatcherError.ItsClientLoadError(_))

  } yield {
    val rejectedOrders = List.empty[(ClientOrder, OrderRejectionReason)]
    val finalModel     = model2
    MatcherOutput(finalModel, rejectedOrders)
  }
}
