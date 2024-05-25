import exchange.domain.model._
import AssetAmounts._

import ClientNames._
import UsdAmounts._

import zio.prelude._

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
