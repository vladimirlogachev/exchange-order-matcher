import ClientName._
import AssetName._
import UsdAmount._
import AssetAmount._
import AssetPrice._

final case class ClientBalances(
    usdBalance: UsdAmount,
    assetBalances: Map[AssetName, AssetAmount]
)

final case class MatcherState(
    balances: Map[ClientName, ClientBalances], // TODO: locked assets and usd
    pendingOrders: List[ClientOrder] // TODO: not a list
)

object MatcherState:
  def empty: MatcherState = MatcherState(balances = Map.empty, pendingOrders = List.empty)

enum ClientLoadError:
  case ClientAlreadyExists

def loadClient(
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
    model: MatcherState
): Either[OrderRejectionReason, MatcherState] =
  clientOrder match {
    case ClientOrder.Buy(clientName, assetName, usdAmount, assetPrice) =>
      for {
        clientBalances <- model.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
        assetBalance <- clientBalances.assetBalances.get(assetName).toRight(OrderRejectionReason.AssetBalanceNotFound)
        _ <-
          if clientBalances.usdBalance >= usdAmount then Right(())
          else Left(OrderRejectionReason.InsufficientUsdBalance)
        // TODO: implement the rest
      } yield model
    case ClientOrder.Sell(clientName, assetName, assetAmount, assetPrice) =>
      for {
        clientBalances <- model.balances.get(clientName).toRight(OrderRejectionReason.ClientNotFound)
        assetBalance <- clientBalances.assetBalances.get(assetName).toRight(OrderRejectionReason.AssetBalanceNotFound)
        _ <- if assetBalance >= assetAmount then Right(()) else Left(OrderRejectionReason.InsufficientAssetBalance)
        // TODO: implement the rest
      } yield model
  }

type MatcherError = ClientLoadError

final case class MatcherOutput(
    state: MatcherState,
    rejectedOrders: List[(ClientOrder, OrderRejectionReason)]
)

def runMatcher(
    clients: List[ClientBalanceRecord], // TODO: stream
    orders: List[ClientOrder] // TODO: stream
): Either[MatcherError, MatcherOutput] = for {
  model1 <- clients
    .foldLeft(Right(MatcherState.empty): Either[ClientLoadError, MatcherState]) { (state, client) =>
      state.flatMap(loadClient(client, _))
    }
} yield {
  val rejectedOrders = List.empty[(ClientOrder, OrderRejectionReason)]
  val finalModel = model1
  MatcherOutput(finalModel, rejectedOrders)
}
