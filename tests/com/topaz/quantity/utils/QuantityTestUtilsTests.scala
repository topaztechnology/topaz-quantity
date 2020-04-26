package com.topaz.quantity.utils

import com.topaz.quantity.UOM._
import com.topaz.quantity.{DblQty, Qty}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class QuantityTestUtilsTests extends AnyFreeSpec with Matchers 
  with QuantityTestUtils
{

  private def q(d: Double) = Qty(d, USD)
  "Tolerance matchers" - {

    "absolute tolerance" in {
      q(10.0) should be      (q(10.09) +- (0.1, 0))
      q(10.0) should be      (q(10.09) +- 0.1)

      q(10.0) should not be  (q(10.09) +- (0.01, 0))
      q(10.0) should not be  (q(10.09) +- 0.01)
    }

    "Mismatched UOMs should fail" in {
      Qty(10, USD) should not be (Qty(10, BBL) +- 1.0)
      Qty(10, USD) should not be (Qty(10, BBL) +- (1.0, 1.0))
    }

    "Fractional differences" in {
      q(10.0) should be (q(10.9) withinFraction (0.1))
      q(10.0) should not be (q(11.1) withinFraction (0.1))
    }

    "Opposite signs can be within abs tolerance" in {
      q(-0.01) should be (q(0.01) +- 0.03)
    }

    "Opposite signs can not be within fractional tolerance" in {
      q(-0.1) should not be (q(0.1) +- (0.0, 100))
    }

    "Zero value can only match absolutely" in {
      q(0) should be (q(0.1) +- 0.1)
      q(0) should be (q(-0.1) +- 0.1)
      q(0) should not be (q(0.1) +- 0.01)

      q(0) should not be (q(0.1) withinFraction 100.0)
    }
  }
}
