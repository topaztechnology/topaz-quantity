package com.topaz.quantity.utils

import com.topaz.maths.ApacheMathsPimps
import com.topaz.quantity.Qty._
import com.topaz.quantity.UOM._
import com.topaz.quantity._
import com.topaz.utils.EitherTestPimps
import org.apache.commons.math3.random.RandomGenerator
import org.scalatest.matchers.{BeMatcher, MatchResult}

import scala.language.implicitConversions

trait QuantityTestUtils extends ApacheMathsPimps with EitherTestPimps {

  private def formatQty(q: Qty, tol_ : Double): String = {
    var dp = 0
    val tol = tol_ max 1e-9
    while (tol * BigDecimal("10").pow(dp) < 1) {
      dp += 1
    }
    q.toFormattedString(dp + 1, useBrackets = false) /* +1 ensures we show how the difference
                                                        doesn't match */
  }

  implicit class WithinToleranceMatcher(rhs_ : Qty) {
    def +- (absTol: Double, fracTol: Double): BeMatcher[Any] = new BeMatcher[Any] {
      require(absTol >= 0.0 && fracTol >= 0, s"Negative tolerances, $absTol or $fracTol")
      override def apply(lhs_ : Any): MatchResult = {
        require (lhs_.isInstanceOf[Qty], s"$lhs_ is not of type qty")

        def percentsToScalar(q: Qty) = if(q.uom == PERCENT)
          Qty(q.doubleValue / 100, SCALAR)
        else
          q
        
        val lhs = percentsToScalar(lhs_.asInstanceOf[Qty]) 
        val rhs = percentsToScalar(rhs_) 

        if (lhs.uom != rhs.uom)
          MatchResult(
            matches = false,
            s"UOMs don't match - actual $lhs vs expected $rhs",
            s""
          )
        else {
          val l = lhs.doubleValue
          val r = rhs.doubleValue
          
          if ((l - r).abs <= absTol)
            MatchResult(matches = true, "", s"$lhs matches $rhs but shouldn't (+- $absTol, fracTol $fracTol)")
          else if (l * r < 0)
            MatchResult(matches = false, "Signs differ " + (lhs, rhs), "Signs same")
          else if (l == 0 || r == 0)
            MatchResult(
              matches = false, 
              s"${formatQty(lhs, absTol)} is not ${formatQty(rhs, absTol)}) +- $absTol", 
              "Signs same"
            )
          else if ((l - r).abs / l.abs <= fracTol)
            MatchResult(matches = true, "", "")
          else
            MatchResult(
              matches = false, 
              s"${formatQty(lhs, fracTol)} is not within $fracTol fraction, abs tol $absTol of ${formatQty(rhs, absTol)}",
              "Match"
            )
        }
      }
    }
    def isWithinTolerances(lhs: Qty, absTol: Double, fracTol: Double): Boolean = {
      (+-(absTol, fracTol))(lhs).matches
    }
    def isWithinAbsoluteTolerance(lhs: Qty, absTol: Double): Boolean = {
      (+-(absTol))(lhs).matches
    }
    def withinFraction(fracTol: Double): BeMatcher[Any] = +- (1e-6, fracTol)
    def +- (absTol: Double): BeMatcher[Any] = +- (absTol, 0.0)
  }

  implicit class PimpedQty(q: Qty) {
    def * (d: Double) = q * Qty(d, SCALAR)
    def / (d: Double) = q / Qty(d, SCALAR)
  }

  implicit class QtyPimpedRandomGenerator(rng: RandomGenerator) {
    def nextQuantity(uom: UOM, min: Double, max: Double, allowZero: Boolean = false): FixedQty = {
      val n = min + rng.nextDouble() * (max - min)
      if (n == 0 && ! allowZero)
        nextQuantity(uom, min, max, allowZero)
      else
        Qty(n, uom)
    }

    def nextPercent(): FreightPrice = nextQuantity(PERCENT, 0, 100)

    def nextIntQuantity(uom: UOM, min: Int, max: Int, allowZero: Boolean = false): FixedQty = {
      val n = min + rng.nextInt(max - min)
      if (n == 0 && ! allowZero)
        nextIntQuantity(uom, min, max, allowZero)
      else
        Qty(n, uom)
    }

    def nextPrice(uom: UOM, min: Double = 15, max: Double = 100, allowZero: Boolean = false): FixedQty = {
      var p = nextDblPrice(uom, min, max).toFixedQty()
      while(!allowZero && p.isZero) p = nextDblPrice(uom, min, max).round(6)
      p
    }

    def nextOilPrice(uom: UOM, min: Int = 35, max: Int = 80,
                     allowZero: Boolean = false, conv: DimensionConversions): FixedQty = {
      // Attempt to generate a somewhat realistic oil price. if we just use nextPrice
      // then USD/MT oil markets get very low prices that don't make sense and don't work for things like USD/BBL
      // stress tests.
      if(uom.priceMajorCCY == USD) {
        val usdBbl = USD / BBL
        var p = nextDblPrice(usdBbl, min, max).round(6)
        while (!allowZero && p.isZero) p = nextDblPrice(usdBbl, min, max).toFixedQty()
        if (uom == usdBbl) {
          p
        } else {
          p.in(uom, conv).right.get.toFixedQty()
        }
      } else {
        nextPrice(uom, min, max, allowZero)
      }
    }

    def nextTradeQuantity(uom: UOM, min: Int = -100, max: Int = 100, allowZero: Boolean = true): FixedQty = {
      nextIntQuantity(uom, min, max, allowZero)
    }

    def nextDblPrice(uom: UOM, min: Double = 15.0, max: Double = 100.0): DblQty = {
      Qty(min + rng.nextDouble() * (max - min), uom)
    }
  }
}

object QuantityTestUtils extends QuantityTestUtils

