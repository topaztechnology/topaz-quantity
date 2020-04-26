package com.topaz.quantity

import com.topaz.quantity.UOM._
import com.topaz.quantity.Qty._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UOMUtilsTest extends AnyFunSuite with Matchers {

  test("distinctConversion for constant conversions") {
    UOMUtils.convertFromTo(1 (BBL), GAL, Seq()) shouldBe Right(42 (GAL))
    UOMUtils.convertFromTo(1 (BBL), GAL, Seq(
      DimensionConversions.fromConversionFactor(1.3),
      DimensionConversions.fromConversionFactor(1.9),
    )) shouldBe Right(42 (GAL))
  }

  test("should fail when it can't convert") {
    UOMUtils.convertFromTo(1 (BBL), MT, Seq()) shouldBe 'left
    UOMUtils.convertFromTo(1 (BBL), M3, Seq(DimensionConversions.fromConversionFactor(7.0))) shouldBe 'left
    UOMUtils.convertFromTo(1 (BBL), MT, Seq(
      DimensionConversions.fromConversionFactor(7.0),
      DimensionConversions.fromConversionFactor(7.5),
    )) shouldBe 'left
  }

  test("should work when it finds a distinct conversion") {
    UOMUtils.convertFromTo(1 (MT), BBL, Seq(DimensionConversions.fromConversionFactor(7.0))) shouldBe
      Right(7 (BBL))
    UOMUtils.convertFromTo(1 (MT), BBL, Seq(
      DimensionConversions.fromConversionFactor(7.0),
      DimensionConversions.fromConversionFactor(7.0),
    )) shouldBe
      Right(7 (BBL))

  }
}
