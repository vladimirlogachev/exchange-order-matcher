import exchange.domain.model._

import zio._
import zio.prelude._
import zio.stream._

import AssetAmounts._
import AssetNames._
import ClientNames._
import UsdAmounts._

final case class ClientBalanceRecord(
    clientName: ClientName,
    usdBalance: UsdAmount,
    balanceA: AssetAmount,
    balanceB: AssetAmount,
    balanceC: AssetAmount,
    balanceD: AssetAmount
)

object ClientBalanceRecord:

  implicit val ClientBalanceRecordEqual: Equal[ClientBalanceRecord] =
    Equal.default

/** Note: A `Set[String]` instead of just `String` can be helpful for lookups and equality checks in unit tests, without
  * a need for sorting the output.
  */
def toFinalBalances(state: MatcherState): Set[ClientBalanceRecord] = state.balances.map {
  case (clientName, clientBalances) =>
    ClientBalanceRecord(
      clientName,
      clientBalances.usdBalance,
      clientBalances.assetBalances(AssetName("A")),
      clientBalances.assetBalances(AssetName("B")),
      clientBalances.assetBalances(AssetName("C")),
      clientBalances.assetBalances(AssetName("D"))
    )
}.toSet

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

enum FileApiError:
  case ItsInputStreamError(e: Throwable) // TODO: remove, it belongs to Cli
  case ItsClientLoadError(e: ClientLoadError)

implicit val FileApiErrorEqual: Equal[FileApiError] =
  Equal.default

final case class FileApiOutput(
    state: MatcherState,
    rejectedOrders: List[(ClientOrder, OrderRejectionReason)]
)

object FileApiOutput:

  implicit val FileApiOutputEqual: Equal[FileApiOutput] =
    Equal.default

def runMatcher(
    clientBalances: ZStream[Any, Throwable, ClientBalanceRecord],
    orders: ZStream[Any, Throwable, ClientOrder]
): IO[FileApiError, FileApiOutput] = {

  for {
    model1 <- clientBalances
      .run(
        ZSink.foldLeft(Right(MatcherState.empty): Either[ClientLoadError, MatcherState])((state, clientBalance) =>
          state.flatMap(loadClientBalance(clientBalance, _))
        )
      )
      .mapError(FileApiError.ItsInputStreamError(_))
    model2 <- ZIO
      .fromEither(model1)
      .mapError(FileApiError.ItsClientLoadError(_))

  } yield {
    val rejectedOrders = List.empty[(ClientOrder, OrderRejectionReason)]
    val finalModel     = model2
    FileApiOutput(finalModel, rejectedOrders)
  }
}

def outputToClientBalanceStrings(output: FileApiOutput): Either[String, Set[String]] =
  toFinalBalances(output.state)
    .map(clientBalanceRecordSyntax.printString(_))
    .foldRight(Right(Set.empty): Either[String, Set[String]])((xE, accE) =>
      for {
        x   <- xE
        acc <- accE
      } yield acc + x
    )
