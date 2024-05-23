object ClientName:
  opaque type ClientName = String

  object ClientName:
    def apply(s: String): ClientName = s
end ClientName

object AssetName:
  opaque type AssetName = String

  object AssetName:
    def apply(s: String): AssetName = s
end AssetName

object UsdAmount:
  opaque type UsdAmount = Int

  object UsdAmount:
    def apply(i: Int): Option[UsdAmount] =
      if i >= 0 then Some(i)
      else None

  extension (x: UsdAmount) def >=(y: UsdAmount): Boolean = x >= y
end UsdAmount

object AssetAmount:
  opaque type AssetAmount = Int

  object AssetAmount:
    def apply(i: Int): Option[AssetAmount] =
      if i >= 0 then Some(i)
      else None

  extension (x: AssetAmount) def >=(y: AssetAmount): Boolean = x >= y
end AssetAmount

object AssetPrice:
  opaque type AssetPrice = Int

  object AssetPrice:
    def apply(i: Int): Option[AssetPrice] =
      if i >= 0 then Some(i)
      else None
end AssetPrice
