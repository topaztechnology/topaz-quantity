package com.topaz.quantity

import com.topaz.utils.EitherTestPimps

trait QtyTestPimps extends EitherTestPimps {

  implicit class QtyPimp(q: Qty) {
    def in(uom: UOM, cf: Double): Qty = {
      q.in(uom, DimensionConversions.fromConversionFactor(cf)).R
    }
  }
}
