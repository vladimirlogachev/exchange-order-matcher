package exchange.domain.model

import exchange.domain.model.AssetAmounts._
import zio.prelude.Equal

enum OrderRejectionReason:
  case ClientNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance
  case InvalidAssetAmount
  case UnexpectedInternalError // TODO: consider removing or defining more specific errors

object OrderRejectionReason:
  implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
    Equal.default
