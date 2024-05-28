package exchange.domain.model

import zio.prelude.Equal

import exchange.domain.model.AssetAmounts._

enum OrderRejectionReason:
  case ClientNotFound
  case InsufficientUsdBalance
  case InsufficientAssetBalance
  case UnexpectedInternalError

object OrderRejectionReason:
  implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
    Equal.default
