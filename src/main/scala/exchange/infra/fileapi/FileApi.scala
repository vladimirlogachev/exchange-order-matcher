package exchange.infra.fileapi

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.ClientNames._
import exchange.domain.model.UsdAmounts._
import exchange.domain.model._
import zio._
import zio.parser.Parser.ParserError
import zio.prelude._
import zio.stream._

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

enum FileApiError:
  case ClientAlreadyExists

implicit val FileApiErrorEqual: Equal[FileApiError] =
  Equal.default

enum StringFileApiError:
  case ItsOtherStreamError(e: Throwable)
  case ItsParserError(e: ParserError[String])
  case ItsPrinterError(e: String)
  case ItsFileApiError(e: FileApiError)

def explainStringFileApiError(e: StringFileApiError): String = e.toString() // TODO: implement

implicit val StringFileApiErrorEqual: Equal[StringFileApiError] =
  Equal.default

final case class FileApiOutput(
    state: MatcherState,
    rejectedOrders: List[(ClientOrder, OrderRejectionReason)]
)

object FileApiOutput:

  implicit val FileApiOutputEqual: Equal[FileApiOutput] =
    Equal.default

object FileApi:

  def run(
      clientBalances: ZStream[Any, StringFileApiError, ClientBalanceRecord],
      orders: ZStream[Any, StringFileApiError, ClientOrder]
  ): IO[StringFileApiError, FileApiOutput] = for {
    state1 <- clientBalances
      .run(
        ZSink.foldLeft(Right(MatcherState.empty): Either[Nothing, MatcherState])((state, clientBalance) =>
          state.flatMap(loadClientBalance(clientBalance, _))
        )
      )

    state2 <- ZIO.fromEither(state1).mapError(StringFileApiError.ItsFileApiError(_))

  } yield {
    val rejectedOrders = List.empty[(ClientOrder, OrderRejectionReason)]
    val finalstate     = state2
    FileApiOutput(finalstate, rejectedOrders)
  }

  def runToBalanceRecords(
      clientBalances: ZStream[Any, StringFileApiError, ClientBalanceRecord],
      orders: ZStream[Any, StringFileApiError, ClientOrder]
  ): IO[StringFileApiError, Set[ClientBalanceRecord]] =
    run(clientBalances, orders).map(x => toFinalBalances(x.state))

  def runFromStrings(
      clientBalances: ZStream[Any, Throwable, String],
      orders: ZStream[Any, Throwable, String]
  ): IO[StringFileApiError, FileApiOutput] =
    run(
      clientBalances
        .mapError(StringFileApiError.ItsOtherStreamError(_))
        .mapZIO(x =>
          ZIO.fromEither(clientBalanceRecordSyntax.parseString(x)).mapError(StringFileApiError.ItsParserError(_))
        ),
      orders
        .mapError(StringFileApiError.ItsOtherStreamError(_))
        .mapZIO(x => ZIO.fromEither(clientOrderSyntax.parseString(x)).mapError(StringFileApiError.ItsParserError(_)))
    )

  def runFromStringsToStrings(
      clientBalances: ZStream[Any, Throwable, String],
      orders: ZStream[Any, Throwable, String]
  ): IO[StringFileApiError, Set[String]] =
    runFromStrings(clientBalances, orders)
      .flatMap(x => ZIO.fromEither(toClientBalanceStrings(x.state)).mapError(StringFileApiError.ItsPrinterError(_)))

  private def loadClientBalance(
      record: ClientBalanceRecord,
      state: MatcherState
  ): Either[FileApiError, MatcherState] =
    if state.balances.contains(record.clientName) then Left(FileApiError.ClientAlreadyExists)
    else
      val clientBalances = Map(
        AssetName("A") -> record.balanceA,
        AssetName("B") -> record.balanceB,
        AssetName("C") -> record.balanceC,
        AssetName("D") -> record.balanceD
      )
      val allBalances = state.balances.updated(record.clientName, ClientBalances(record.usdBalance, clientBalances))
      Right(state.copy(balances = allBalances))

  /** Note: A `Set[String]` instead of just `String` can be helpful for lookups and equality checks in unit tests,
    * without a need for sorting the output.
    */
  private def toFinalBalances(state: MatcherState): Set[ClientBalanceRecord] = state.balances.map {
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

  private def toClientBalanceStrings(state: MatcherState): Either[String, Set[String]] =
    toFinalBalances(state)
      .map(clientBalanceRecordSyntax.printString(_))
      .foldRight(Right(Set.empty): Either[Nothing, Set[String]])((xE, accE) =>
        for {
          x   <- xE
          acc <- accE
        } yield acc + x
      )
