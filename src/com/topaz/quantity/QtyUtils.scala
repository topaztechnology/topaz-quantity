package com.topaz.quantity

import scala.math.BigDecimal

object QtyUtils {
  // I had to move these from Qty as we were getting scala static initialisation deadlock
  val BDOne = BigDecimal(1)
  val SDOne = SmallDecimal.of(1)
}
