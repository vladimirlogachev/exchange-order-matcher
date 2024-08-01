package exchange.infra.fileapi

import zio._
import zio.parser.StringParserError
import zio.prelude.Equal
import zio.stream._

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.ClientBalance
import exchange.domain.model.ClientNames._
import exchange.domain.model.CompoundBalance
import exchange.domain.model.ExchangeState
import exchange.domain.model.Order
import exchange.domain.model.OrderRejectionReason
import exchange.domain.model.UsdAmounts._

/** Describes the input (`clients.txt`) and output (`results.txt`) client balances
  */
private final case class ClientBalanceRecord(
    clientName: ClientName,
    usdBalance: UsdAmount,
    balanceA: AssetAmount,
    balanceB: AssetAmount,
    balanceC: AssetAmount,
    balanceD: AssetAmount
)

private object ClientBalanceRecord:
  implicit val ClientBalanceRecordEqual: Equal[ClientBalanceRecord] =
    Equal.default

private enum FileApiError:
  case ClientAlreadyExists

private object FileApiError:
  implicit val FileApiErrorEqual: Equal[FileApiError] =
    Equal.default

private def explainFileApiError(err: FileApiError): String = err match
  case FileApiError.ClientAlreadyExists => "Client already exists"

enum StringFileApiError:
  case ItsOtherStreamError(e: Throwable)
  case ItsParserError(e: StringParserError[String])
  case ItsPrinterError(e: String)
  case ItsFileApiError(e: FileApiError)

def explainStringFileApiError(err: StringFileApiError): String = err match
  case StringFileApiError.ItsOtherStreamError(e) => s"Stream error: ${e.getMessage}"
  case StringFileApiError.ItsParserError(e)      => explainStringParserError(e)
  case StringFileApiError.ItsPrinterError(e) => s"""|Failed to print:
                                                    |$e""".stripMargin
  case StringFileApiError.ItsFileApiError(e) => explainFileApiError(e)

implicit val StringFileApiErrorEqual: Equal[StringFileApiError] =
  Equal.default

private final case class FileApiOutput(
    state: ExchangeState,
    rejectedOrders: Vector[(Order, OrderRejectionReason)]
)

private object FileApiOutput:
  implicit val FileApiOutputEqual: Equal[FileApiOutput] =
    Equal.default

object FileApi:
  /** The most simple and straightforward way to use the API, without the need to deal with the details.
    */
  def runFromStringsToStrings(
      clientBalances: ZStream[Any, Throwable, String],
      orders: ZStream[Any, Throwable, String]
  ): IO[StringFileApiError, Set[String]] =
    runFromStrings(clientBalances, orders)
      .flatMap(x => ZIO.fromEither(toClientBalanceStrings(x.state)).mapError(StringFileApiError.ItsPrinterError(_)))

  /** Takes strings, outputs the detailed result.
    */
  def runFromStrings(
      clientBalances: ZStream[Any, Throwable, String],
      orders: ZStream[Any, Throwable, String]
  ): IO[StringFileApiError, FileApiOutput] =
    run(
      clientBalances
        .mapError(StringFileApiError.ItsOtherStreamError(_))
        .mapZIO(s =>
          ZIO
            .fromEither(clientBalanceRecordSyntax.parseString(s))
            .mapError(StringFileApiError.ItsParserError(_))
        ),
      orders
        .mapError(StringFileApiError.ItsOtherStreamError(_))
        .mapZIO(s =>
          ZIO
            .fromEither(orderSyntax.parseString(s))
            .mapError(StringFileApiError.ItsParserError(_))
        )
    )

  /** Takes the successfully parsed balances and orders, outputs the detailed result.
    */
  def run(
      clientBalances: ZStream[Any, StringFileApiError, ClientBalanceRecord],
      orders: ZStream[Any, StringFileApiError, Order]
  ): IO[StringFileApiError, FileApiOutput] = for {
    state1Either <- clientBalances
      .run(
        ZSink.foldLeft(Right(ExchangeState.empty): Either[Nothing, ExchangeState])((state, clientBalance) =>
          state.flatMap(loadClientBalance(clientBalance, _))
        )
      )

    state1 <- ZIO.fromEither(state1Either).mapError(StringFileApiError.ItsFileApiError(_))

    (finalState, rejectedOrders) <- orders
      .run(
        ZSink.foldLeft((state1, Vector.empty[(Order, OrderRejectionReason)]))((acc, order) =>
          val (state, rejectedOrders) = acc
          state.processOrder(order) match
            case Right(newState) => (newState, rejectedOrders)
            case Left(rejection) => (state, rejectedOrders :+ (order, rejection))
        )
      )
  } yield FileApiOutput(finalState, rejectedOrders)

  /** Loads a single client balance from the record into the state.
    */
  private def loadClientBalance(
      record: ClientBalanceRecord,
      state: ExchangeState
  ): Either[FileApiError, ExchangeState] =
    if state.balances.contains(record.clientName) then Left(FileApiError.ClientAlreadyExists)
    else
      val clientBalances = Map(
        AssetName("A") -> CompoundBalance(free = record.balanceA, locked = AssetAmount.zero),
        AssetName("B") -> CompoundBalance(free = record.balanceB, locked = AssetAmount.zero),
        AssetName("C") -> CompoundBalance(free = record.balanceC, locked = AssetAmount.zero),
        AssetName("D") -> CompoundBalance(free = record.balanceD, locked = AssetAmount.zero)
      )
      val allBalances = state.balances.updated(
        record.clientName,
        ClientBalance(CompoundBalance(free = record.usdBalance, locked = UsdAmount.zero), clientBalances)
      )
      Right(state.copy(balances = allBalances))

  /** Note: A `Set[String]` instead of just `String` can be helpful for lookups and equality checks in unit tests,
    * without a need for sorting the output.
    */
  private def toFinalBalances(state: ExchangeState): Set[ClientBalanceRecord] = state.balances.map {
    case (clientName, clientBalances) =>
      ClientBalanceRecord(
        clientName,
        CompoundBalance.totalUsdBalance(clientBalances.usdBalance),
        clientBalances.assetBalances
          .get(AssetName("A"))
          .map(CompoundBalance.totalAssetBalance)
          .getOrElse(AssetAmount.zero),
        clientBalances.assetBalances
          .get(AssetName("B"))
          .map(CompoundBalance.totalAssetBalance)
          .getOrElse(AssetAmount.zero),
        clientBalances.assetBalances
          .get(AssetName("C"))
          .map(CompoundBalance.totalAssetBalance)
          .getOrElse(AssetAmount.zero),
        clientBalances.assetBalances
          .get(AssetName("D"))
          .map(CompoundBalance.totalAssetBalance)
          .getOrElse(AssetAmount.zero)
      )
  }.toSet

  /** Converts the final balances to a set of strings.
    */
  private def toClientBalanceStrings(state: ExchangeState): Either[String, Set[String]] =
    toFinalBalances(state)
      .map(clientBalanceRecordSyntax.printString(_))
      .foldLeft(Right(Set.empty): Either[Nothing, Set[String]])((accE, xE) =>
        for {
          x   <- xE
          acc <- accE
        } yield acc + x
      )

  /** A helper for the ease of testing and debugging.
    */
  def toSimplifiedRejectedOrders(
      output: FileApiOutput
  ): Either[StringFileApiError, Vector[(String, OrderRejectionReason)]] =
    output.rejectedOrders
      .map { case (x, reason) =>
        (orderSyntax.printString(x).left.map(StringFileApiError.ItsPrinterError(_)), reason)
      }
      .foldLeft(Right(Vector.empty): Either[StringFileApiError, Vector[(String, OrderRejectionReason)]])((accE, elem) =>
        val (xE, reason) = elem
        for {
          x   <- xE
          acc <- accE
        } yield acc :+ (x, reason)
      )
