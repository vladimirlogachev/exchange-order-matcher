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

def explainFileApiError(err: FileApiError): String = err match
  case FileApiError.ClientAlreadyExists => "Client already exists"

enum StringFileApiError:
  case ItsOtherStreamError(e: Throwable)
  case ItsParserError(s: String, e: ParserError[String])
  case ItsPrinterError(e: String)
  case ItsFileApiError(e: FileApiError)

def explainStringFileApiError(err: StringFileApiError): String = err match
  case StringFileApiError.ItsOtherStreamError(e) => s"Stream error: ${e.getMessage}"
  case StringFileApiError.ItsParserError(s, e) => s"""|Failed to parse: $s
                                                      |${explainParserError(e)}""".stripMargin

  case StringFileApiError.ItsPrinterError(e) => s"""|Failed to print:
                                                    |$e""".stripMargin

  case StringFileApiError.ItsFileApiError(e) => explainFileApiError(e)

implicit val StringFileApiErrorEqual: Equal[StringFileApiError] =
  Equal.default

final case class FileApiOutput(
    state: ExchangeState,
    rejectedOrders: Vector[(Order, OrderRejectionReason)]
)

object FileApiOutput:

  implicit val FileApiOutputEqual: Equal[FileApiOutput] =
    Equal.default

object FileApi:

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

    // _ <- {
    //   // TODO: remove
    //   for {
    //     _ <- Console.printLine("\nBuy orders for A:")
    //     _ <- Console.printLine(finalState.orders(AssetName("A")).buyOrders.map(_.toString).mkString("\n"))

    //     _ <- Console.printLine("\nSell orders for A:")
    //     _ <- Console.printLine(finalState.orders(AssetName("A")).sellOrders.map(_.toString).mkString("\n"))

    //     _ <- Console.printLine("\nRejected orders:")
    //     _ <- Console.printLine(rejectedOrders.map(_.toString).mkString("\n"))
    //   } yield ()
    // }.mapError(e => StringFileApiError.ItsOtherStreamError(e))

  } yield FileApiOutput(finalState, rejectedOrders)

  def runToBalanceRecords(
      clientBalances: ZStream[Any, StringFileApiError, ClientBalanceRecord],
      orders: ZStream[Any, StringFileApiError, Order]
  ): IO[StringFileApiError, Set[ClientBalanceRecord]] =
    run(clientBalances, orders).map(x => toFinalBalances(x.state))

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
            .mapError(e => StringFileApiError.ItsParserError(s, e))
        ),
      orders
        .mapError(StringFileApiError.ItsOtherStreamError(_))
        .mapZIO(s =>
          ZIO
            .fromEither(orderSyntax.parseString(s))
            .mapError(e => StringFileApiError.ItsParserError(s, e))
        )
    )

  def runFromStringsToStrings(
      clientBalances: ZStream[Any, Throwable, String],
      orders: ZStream[Any, Throwable, String]
  ): IO[StringFileApiError, Set[String]] =
    runFromStrings(clientBalances, orders)
      .flatMap(x => ZIO.fromEither(toClientBalanceStrings(x.state)).mapError(StringFileApiError.ItsPrinterError(_)))

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

  private def toClientBalanceStrings(state: ExchangeState): Either[String, Set[String]] =
    toFinalBalances(state)
      .map(clientBalanceRecordSyntax.printString(_))
      .foldLeft(Right(Set.empty): Either[Nothing, Set[String]])((accE, xE) =>
        for {
          x   <- xE
          acc <- accE
        } yield acc + x
      )

  def toSimplifiedRejectedOrders(
      output: FileApiOutput
  ): Either[StringFileApiError, Vector[(String, OrderRejectionReason)]] =
    output.rejectedOrders
      .map { case (x, reason) =>
        (orderSyntax.printString(x).leftMap(StringFileApiError.ItsPrinterError(_)), reason)
      }
      .foldLeft(Right(Vector.empty): Either[StringFileApiError, Vector[(String, OrderRejectionReason)]])((accE, elem) =>
        val (xE, reason) = elem
        for {
          x   <- xE
          acc <- accE
        } yield acc :+ (x, reason)
      )
