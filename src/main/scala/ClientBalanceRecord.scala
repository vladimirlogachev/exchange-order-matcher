import zio.parser._

import ClientName._
import AssetName._
import UsdAmount._
import AssetAmount._
import AssetPrice._

final case class ClientBalanceRecord(
    clientName: ClientName,
    usdBalance: UsdAmount,
    balanceA: AssetAmount,
    balanceB: AssetAmount,
    balanceC: AssetAmount,
    balanceD: AssetAmount
)

object ClientBalanceRecord:
  val syntax = {
    val tupleSyntax =
      ClientName.syntax
        ~ tabChar
        ~ UsdAmount.syntax
        ~ tabChar
        ~ AssetAmount.syntax
        ~ tabChar
        ~ AssetAmount.syntax
        ~ tabChar
        ~ AssetAmount.syntax
        ~ tabChar
        ~ AssetAmount.syntax

    tupleSyntax.transform(
      ClientBalanceRecord.apply.tupled,
      x => (x.clientName, x.usdBalance, x.balanceA, x.balanceB, x.balanceC, x.balanceD)
    ) ?? "ClientBalanceRecord"
  }
