package com.topaz.quantity

object RoundPrice {
  /*
  A little weird passing in a boolean that causes this function to do nothing,
  but less verbose than any alternative I could think of
   */
  def apply(price: Qty, dps: Int, doRounding: Boolean): Qty = {
    if (doRounding)
      price.round(dps)
    else
      price
  }

}

