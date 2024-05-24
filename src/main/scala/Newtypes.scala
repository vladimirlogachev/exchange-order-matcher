import zio.parser._

val tabChar = Syntax.char('\t')

object ClientName:
  opaque type ClientName = String

  object ClientName:
    def apply(s: String): ClientName = s

    val syntax: Syntax[String, Char, Char, ClientName] = Syntax.notChar('\t').repeat.string

object AssetName:
  opaque type AssetName = String

  object AssetName:
    def apply(s: String): AssetName = s

    val syntax: Syntax[String, Char, Char, AssetName] = Syntax.notChar('\t').repeat.string

object UsdAmount:
  opaque type UsdAmount = Int

  object UsdAmount:
    def apply(i: Int): Option[UsdAmount] =
      if i >= 0 then Some(i)
      else None

    val syntax: Syntax[String, Char, Char, UsdAmount] = Syntax.digit.repeat.string.transformEither(
      _.toIntOption.flatMap(apply(_)).toRight("Not a valid USD amount"),
      v => Right(v.toString)
    ) ?? "UsdAmount"

  extension (x: UsdAmount) def >=(y: UsdAmount): Boolean = x >= y

object AssetAmount:
  opaque type AssetAmount = Int

  object AssetAmount:
    def apply(i: Int): Option[AssetAmount] =
      if i >= 0 then Some(i)
      else None

    val syntax: Syntax[String, Char, Char, AssetAmount] = Syntax.digit.repeat.string.transformEither(
      _.toIntOption.flatMap(apply(_)).toRight("Not a valid asset amount"),
      v => Right(v.toString)
    ) ?? "AssetAmount"

  extension (x: AssetAmount) def >=(y: AssetAmount): Boolean = x >= y

object AssetPrice:
  opaque type AssetPrice = Int

  object AssetPrice:
    def apply(i: Int): Option[AssetPrice] =
      if i > 0 then Some(i)
      else None

    val syntax: Syntax[String, Char, Char, AssetPrice] = Syntax.digit.repeat.string.transformEither(
      _.toIntOption.flatMap(apply(_)).toRight("Not a valid asset price"),
      v => Right(v.toString)
    ) ?? "AssetPrice"
