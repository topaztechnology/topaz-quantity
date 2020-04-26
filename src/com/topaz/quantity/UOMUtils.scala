package com.topaz.quantity

import com.topaz.utils.TopazFail

object UOMUtils {
  /**
    * Returns the single conversion between `from` and `to` or a Left if there are multiple
    * matching conversions in `conversions`
    */
  def convertFromTo(
    from: Qty, to: UOM, conversions: Seq[DimensionConversions]
  ): Either[TopazFail, Qty] = {

    if (from.uom == to) {
      Right(from)
    } else if (from.uom.hasConstantConversion(to)) {
      from.in(to)
    } else {
      conversions match {
        case Seq() =>
          TopazFail(s"No conversions specified and no constant conversion from $from to $to")
        case _ =>
          val (rights, _) = conversions.map(from.in(to, _)).partition(_.isRight)
          val distinctRights = rights.distinct
          if (distinctRights.size > 1)
            TopazFail(s"Non-unique conversions of $from to $to")
          else if (distinctRights.isEmpty)
            TopazFail(s"No conversions of $from to $to")
          else
            Right(rights.head.right.get)
      }
    }
  }
}
