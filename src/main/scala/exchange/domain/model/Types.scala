package exchange.domain.model

object ClientNames:
  opaque type ClientName = String

  object ClientName:
    def apply(s: String): ClientName  = s
    def unwrap(s: ClientName): String = s

object AssetNames:
  opaque type AssetName = String

  object AssetName:
    def apply(s: String): AssetName  = s
    def unwrap(s: AssetName): String = s

object UsdAmounts:
  opaque type UsdAmount = Int

  extension (x: UsdAmount) def >=(y: UsdAmount): Boolean = x >= y

  object UsdAmount:

    def apply(i: Int): Option[UsdAmount] =
      if i >= 0 then Some(i)
      else None

object AssetAmounts:
  opaque type AssetAmount = Int

  extension (x: AssetAmount) def >=(y: AssetAmount): Boolean = x >= y

  object AssetAmount:

    def apply(i: Int): Option[AssetAmount] =
      if i >= 0 then Some(i)
      else None

object AssetPrices:
  opaque type AssetPrice = Int

  object AssetPrice:

    def apply(i: Int): Option[AssetPrice] =
      if i > 0 then Some(i)
      else None