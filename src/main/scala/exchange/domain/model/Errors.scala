package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import zio.prelude.Equal

enum OrderRejectionReason:
  case ClientNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance
  case UnexpectedInternalError

object OrderRejectionReason:
  implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
    Equal.default
