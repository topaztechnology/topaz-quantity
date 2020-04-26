package com.topaz.quantity

import com.topaz.performance.{RegressionTest, RegressionTestSuite}
import com.topaz.quantity.UOM._
import com.topaz.utils.Stopwatch

case class QuantityAdditionRegressionTest(
  override val name: String, initial: Qty, increment: Qty, numIter: Int
) 
  extends RegressionTest(name, s"Adds $numIter quantities")
{
  def test() {
    var q: Qty = initial
    for (i <- 1 to numIter)
      q = q + increment
  }

  override def jvmargs = Vector("-Xmx50m")
}

case class QuantityMultiplicationRegressionTest(
  override val name: String, initial: Qty, multiplier: Qty, numIter: Int
) 
  extends RegressionTest(name, s"Multiplies $numIter quantities")
{
  def test() {
    var q: Qty = initial
    for (i <- 1 to numIter / 2) {
      q = q / multiplier
      q = q * multiplier
    }
  }

  override def jvmargs = Vector("-Xmx50m")
}


// For testing from vim/idea
object QuantityPerformanceTest extends App {
  val sw = Stopwatch()
  new QuantityPerformanceSuite().tests.foreach{
    test => 
      sw.restart
      test.test()
      println(s"${test.name} took $sw")
  }
}

class QuantityPerformanceSuite extends RegressionTestSuite {

  // Iterations chosen so that each test took roughly 
  // one second to run
  
  val doubleQtyAddition = QuantityAdditionRegressionTest(
    "Add 15M DblQty", 
    Qty(1.0, MT), Qty(1.0, MT), 
    15 * 1000 * 1000)

  val sdQtyAddition = QuantityAdditionRegressionTest(
    "Add 60M SDQty", 
    Qty(1, MT), Qty(1, MT), 
    60 * 1000 * 1000)

  val bdQtyAddition = QuantityAdditionRegressionTest(
    "Add 8M BDQty", 
    Qty("1.00000000001", MT), Qty("1.0000000001", MT), 
    8 * 1000 * 1000)

  val doubleAdditionWithUOMConversion = new QuantityAdditionRegressionTest(
    "Add 300K DblQty with UOM conversion", 
    Qty(1.0, USD), Qty(1.0, US_CENT), 
    300 * 1000
  ) {
    override def maxRegressionFraction: Double = 0.3      /* This was regularly failing - seems this test reliably 
                                                             runs in either 0.9 or 1.1 seconds! */
                                
  }

  val doubleQtyMultiplication = QuantityMultiplicationRegressionTest(
    "Multiply 7M DblQty", 
    Qty(1.0, MT), Qty(3.0, USD), 
    7 * 1000 * 1000)

  val sdQtyMultiplication = QuantityMultiplicationRegressionTest(
    "Multiply 1.5M SDQty", 
    Qty(1, MT), Qty(3, GBP), 
    1500 * 1000)

  val bdQtyMultiplication = QuantityMultiplicationRegressionTest(
    "Multiply 1M BDQty", 
    Qty("1.00000000001", MT), Qty("1.0000000001", USD), 
    1 * 1000 * 1000)

  def tests = Vector(
    doubleQtyAddition,
    sdQtyAddition,
    bdQtyAddition,
    doubleAdditionWithUOMConversion,
    doubleQtyMultiplication,
    sdQtyMultiplication,
    bdQtyMultiplication
  )
}
