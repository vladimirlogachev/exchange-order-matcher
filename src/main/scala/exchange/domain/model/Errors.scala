package exchange.domain.model

import zio.prelude.Equal

enum OrderRejectionReason:
  case ClientNotFound
  case UnknownAsset
  case InsufficientUsdBalance
  case InsufficientAssetBalance
  case UnexpectedInternalError

object OrderRejectionReason:
  implicit val OrderRejectionReasonEqual: Equal[OrderRejectionReason] =
    Equal.default
