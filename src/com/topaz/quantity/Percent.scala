package com.topaz.quantity

import com.topaz.quantity.Qty.Percent
import com.topaz.quantity.UOM.PERCENT


object Percent {
  def apply(valueInPercent: Double): Qty = apply(valueInPercent, Taint.clean)

  def apply(valueInPercent: Double, taint: Taint): Qty = new DblQty(valueInPercent, PERCENT, taint)

  def apply(valueInPercent: Int): FixedQty = Qty(valueInPercent, PERCENT, Taint.clean)

  def apply(valueInPercent: BigDecimal): FixedQty = apply(valueInPercent, Taint.clean)
  def apply(valueInPercent: BigDecimal, taint: Taint): FixedQty =
    SmallDecimal.maybe(valueInPercent.bigDecimal).map(Qty(_, PERCENT, taint)).getOrElse(
      new BDQty(valueInPercent, PERCENT, taint)
    )

  def apply(valueInPercent: String): FixedQty = apply(BigDecimal(valueInPercent))

  def unapply(str: String): Option[FixedQty] = {
    Qty.unapply(str) match {
      case Some(qty) if qty.isScalarCategory => Some(qty)
      case _ => None
    }
  }


  /**
    * e.g. for use with Excel. Excel represents percentages as doubles where 0.5 is 50%
    * This method rounds to 9dp to remove noise from the double.
    */
  def fromDouble(pc: Double, taint: Taint = Taint.clean): Percent = Qty(BigDecimal(pc) * 100, PERCENT, taint).round(9)

  val ZERO = apply(0)
}



