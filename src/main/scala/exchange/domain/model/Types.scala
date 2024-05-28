package exchange.domain.model

import zio.prelude.Equal

object ClientNames:
  opaque type ClientName = String

  object ClientName:
    implicit val ClientNameEqual: Equal[ClientName] = Equal.default
    def apply(s: String): ClientName                = s
    def unwrap(s: ClientName): String               = s

object AssetNames:
  opaque type AssetName = String

  object AssetName:
    def apply(s: String): AssetName  = s
    def unwrap(s: AssetName): String = s

object UsdAmounts:
  /** Note: Must be greater than or equal to 0
    */
  opaque type UsdAmount = Int

  extension (x: UsdAmount) {
    def +(y: UsdAmount): UsdAmount         = x + y
    def -(y: UsdAmount): Option[UsdAmount] = UsdAmount(x - y)
    def >=(y: UsdAmount): Boolean          = x >= y
  }

  object UsdAmount:
    def zero: UsdAmount = 0

    def apply(i: Int): Option[UsdAmount] =
      if i >= 0 then Some(i)
      else None

object AssetAmounts:
  /** Note: Must be greater than or equal to 0
    */
  opaque type AssetAmount = Int

  extension (x: AssetAmount) {
    def toUsdAmount(p: AssetPrices.AssetPrice) = UsdAmounts.UsdAmount(p.unwrap * x)
    def +(y: AssetAmount): AssetAmount         = x + y
    def -(y: AssetAmount): Option[AssetAmount] = AssetAmount(x - y)
    def >=(y: AssetAmount): Boolean            = x >= y
    def >(y: AssetAmount): Boolean             = x > y
    def <(y: AssetAmount): Boolean             = x < y
  }

  object AssetAmount:
    implicit val AssetAmountEqual: Equal[AssetAmount] = Equal.default

    def zero: AssetAmount = 0

    def apply(i: Int): Option[AssetAmount] =
      if i >= 0 then Some(i)
      else None

    def min(x: AssetAmount, y: AssetAmount): AssetAmount = Math.min(x, y)

object OrderAmounts:

  /** Note: Must be greater than 0
    */
  opaque type OrderAmount = AssetAmounts.AssetAmount

  extension (x: OrderAmount) {
    def toAssetAmount: AssetAmounts.AssetAmount = x
  }

  object OrderAmount:
    implicit val OrderAmountEqual: Equal[OrderAmount] = Equal.default

    def fromAssetAmount(x: AssetAmounts.AssetAmount): Option[OrderAmount] =
      AssetAmounts.AssetAmount(1).flatMap(one => if x >= one then Some(x) else None)

    def apply(i: Int): Option[OrderAmount] =
      if i >= 1 then AssetAmounts.AssetAmount(i)
      else None

    def min(x: OrderAmount, y: OrderAmount): OrderAmount = AssetAmounts.AssetAmount.min(x, y)

object AssetPrices:
  /** Note: Must be greater than 0
    */
  opaque type AssetPrice = Int

  extension (x: AssetPrice) {
    def unwrap: Int = x

    def <=(y: AssetPrice): Boolean = x <= y
    def >=(y: AssetPrice): Boolean = x >= y
  }

  object AssetPrice:
    given Ordering[AssetPrice] with
      def compare(x: AssetPrice, y: AssetPrice): Int = x.compare(y)

    def apply(i: Int): Option[AssetPrice] =
      if i > 0 then Some(i)
      else None
