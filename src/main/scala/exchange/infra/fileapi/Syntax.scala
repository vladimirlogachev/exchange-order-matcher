package exchange.infra.fileapi

/** This module contains the syntax definitions for parsing the input files.
  */

import scala.util.control.TailCalls._

import zio.parser.Parser.ParserError
import zio.parser._

import exchange.domain.model.AssetAmounts._
import exchange.domain.model.AssetNames._
import exchange.domain.model.AssetPrices._
import exchange.domain.model.ClientNames._
import exchange.domain.model.Order
import exchange.domain.model.OrderAmounts._
import exchange.domain.model.OrderSide
import exchange.domain.model.UsdAmounts._

/** Transforms a parser error into a human-readable string for CLI output.
  */
def explainStringParserError(spe: StringParserError[String]): String =
  s"""|Parsing Error:
      |Input: ${spe.input}
      |${explainParserError(spe.error).result}""".stripMargin

/** Transforms a parser error into a human-readable string for CLI output.
  */
private def explainParserError(err: ParserError[String]): TailRec[String] = err match
  case ParserError.Failure(nameStack, position, failure) =>
    done(s"""|Name: ${nameStack.reverse.mkString(".")}
             |Position: $position
             |Reason: $failure""".stripMargin)

  case ParserError.UnknownFailure(nameStack, position) =>
    done(s"""|Name: ${nameStack.reverse.mkString(".")}
             |Position: $position
             |Reason: Unknown failure""".stripMargin)

  case ParserError.UnexpectedEndOfInput(nameStack) =>
    done(s"""|Name: ${nameStack.reverse.mkString(".")}
             |Reason: Unexpected end of input""".stripMargin)

  case ParserError.NotConsumedAll(nameStack, position) =>
    done(s"""|Name: ${nameStack.reverse.mkString(".")}
             |Position: $position
             |Reason: Not consumed all""".stripMargin)

  case ParserError.AllBranchesFailed(left, right) =>
    for {
      leftResult  <- tailcall(explainParserError(left))
      rightResult <- tailcall(explainParserError(right))
    } yield s"""|Reason: All branches failed
                |Left:
                |${leftResult}
                |
                |Right:
                |${rightResult}""".stripMargin

val tabChar = Syntax.char('\t')

val clientNameSyntax: Syntax[String, Char, Char, ClientName] =
  Syntax.notChar('\t').repeat.string.transform(ClientName(_), ClientName.unwrap)

val assetNameSyntax: Syntax[String, Char, Char, AssetName] =
  Syntax.notChar('\t').repeat.string.transform(AssetName(_), AssetName.unwrap)

val usdAmountSyntax: Syntax[String, Char, Char, UsdAmount] = Syntax.digit.repeat.string.transformEither(
  UsdAmount.fromString(_).toRight("Not a valid USD amount"),
  v => Right(v.toString)
) ?? "UsdAmount"

val assetAmountSyntax: Syntax[String, Char, Char, AssetAmount] = Syntax.digit.repeat.string.transformEither(
  AssetAmount.fromString(_).toRight("Not a valid asset amount"),
  v => Right(v.toString)
) ?? "AssetAmount"

val assetPriceSyntax: Syntax[String, Char, Char, AssetPrice] = Syntax.digit.repeat.string.transformEither(
  AssetPrice.fromString(_).toRight("Not a valid asset price"),
  v => Right(v.toString)
) ?? "AssetPrice"

val orderAmountSyntax: Syntax[String, Char, Char, OrderAmount] = Syntax.digit.repeat.string.transformEither(
  OrderAmount.fromString(_).toRight("Not a valid order amount"),
  v => Right(v.toString)
) ?? "OrderAmount"

val orderSideSyntax =
  (Syntax.char('b').as(OrderSide.Buy) ?? "Buy" | Syntax.char('s').as(OrderSide.Sell) ?? "Sell") ?? "OrderSide"

val orderSyntax: Syntax[String, Char, Char, Order] = {
  val tupleSyntax =
    clientNameSyntax
      ~ tabChar
      ~ orderSideSyntax
      ~ tabChar
      ~ assetNameSyntax
      ~ tabChar
      ~ orderAmountSyntax
      ~ tabChar
      ~ assetPriceSyntax
      ~ Syntax.end

  tupleSyntax.transformTo(
    Order.apply.tupled,
    x => (x.clientName, x.side, x.assetName, x.amount, x.assetPrice),
    "Not an Order"
  ) ?? "Order"
}

val clientBalanceRecordSyntax = {
  val tupleSyntax =
    clientNameSyntax
      ~ tabChar
      ~ usdAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ tabChar
      ~ assetAmountSyntax
      ~ Syntax.end

  tupleSyntax.transform(
    ClientBalanceRecord.apply.tupled,
    x => (x.clientName, x.usdBalance, x.balanceA, x.balanceB, x.balanceC, x.balanceD)
  ) ?? "ClientBalanceRecord"
}
