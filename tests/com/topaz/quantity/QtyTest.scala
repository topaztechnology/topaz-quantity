package com.topaz.quantity

import com.topaz.quantity.Qty._
import com.topaz.quantity.UOM._
import com.topaz.quantity.utils.QuantityTestUtils
import com.topaz.utils.{CollectionUtils, EitherPimpsMixin, EitherTestPimps, ScalaTestUtils}
import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class QtyTest extends AnyFunSuite with Matchers
  with EitherValues
  with EitherTestPimps
  with EitherPimpsMixin
  with ScalaTestUtils
  with QuantityTestUtils
  with CollectionUtils {

  test("add") {
    Qty(1, USD) + Qty(3, USD) shouldEqual Qty(4, USD)
    Qty(1, USD).plus(Qty(3, USD)) shouldEqual Qty(4, USD) // 'plus' enforces FixedQty type

    Qty(1, USD) - Qty(3, USD) shouldEqual Qty(-2, USD)
    Qty(1, USD) - Qty(2, US_CENT) shouldEqual Qty(.98, USD)
    Qty(1, US_CENT) + Qty(1, USD) shouldEqual Qty(101, US_CENT)
    Qty(1.0, US_CENT) + Qty(1.0, USD) shouldEqual Qty(101.0, US_CENT)
    Qty(1.0, US_CENT) + -Qty(1.0, USD) shouldEqual Qty(-99.0, US_CENT)
    Qty(1.0, MT) + Qty(1.0, KT) shouldEqual Qty(1001, MT)
  }

  test("add can fail") {

    intercept[RuntimeException] {
      Qty(1, USD) + Qty(1, BBL)
    }
    intercept[RuntimeException] {
      Qty(1, USD) - Qty(1, BBL)
    }
    intercept[RuntimeException] {
      Qty("1", USD) - Qty(1, BBL)
    }
    intercept[RuntimeException] {
      Qty(1.0, US_CENT) + Qty(1.0, BBL)
    }
  }

  test("checked values") {
    Qty(7, USD).checkedDouble(USD) shouldEqual 7
    Qty(7, USD).checkedBDValue(USD) shouldEqual BigDecimal(7)

    intercept[IllegalArgumentException] {
      Qty(1, USD).checkedDouble(BBL)
    }
    intercept[IllegalArgumentException] {
      Qty(1, USD).checkedBDValue(BBL)
    }
  }

  test("add bigdecimal") {
    Qty(".1", USD) + Qty("3", USD) shouldEqual Qty("3.1", USD).ensuringFixedPoint
    Qty("1", USD) - Qty("3", USD) shouldEqual Qty("-2", USD).ensuringFixedPoint
    assert((Qty(".1", USD) + Qty("3", USD)).isFixedPoint === true)
    assert((Qty(".1", USD) + Qty(3.0, USD)).isFixedPoint === false)
    assert((Qty(.1, USD) + Qty(3, USD)).isFixedPoint === false)
    assert((Qty(.1, USD) + Qty("3", USD)).isFixedPoint === false)

    intercept[RuntimeException] {
      (Qty(.1, USD) + Qty("3", USD)).ensuringFixedPoint
    }
  }

  test("mult/div") {
    intercept[ArithmeticException](Qty(2.0, USD) / Qty.NULL).getMessage should include (
      "Division by zero"
    )
    intercept[ArithmeticException](Qty(2, USD) / Qty(0, SCALAR)).getMessage should include (
      "Division by zero"
    )

    Qty(2, USD) * Qty(3, USD) shouldEqual Qty(6, USD * USD)
    Qty(2, USD).mult(Qty(3, USD)) shouldEqual Qty(6, USD * USD) // 'mult' enforces FixedQty type
    Qty(2, USD) * Qty(3.0, USD) shouldEqual Qty(6, USD * USD)
    Qty("2.0000005", USD) * Qty(2, USD) shouldEqual Qty("4.000001", USD * USD)
    Qty("2.0000005", USD) * Qty(2, USD).invert shouldEqual Qty("1.00000025", SCALAR)
    Qty(2, USD) / Qty(4, BBL) shouldEqual Qty(.5, USD / BBL)
    Qty("2", USD) * Qty("3", USD) shouldEqual Qty("6", USD * USD).ensuringFixedPoint
    Qty("2", USD) / Qty("4", BBL) shouldEqual Qty(".5", USD / BBL).ensuringFixedPoint
    (Qty(1, USD) * Qty(4, GAL)) / Qty(4, BBL) should be (Qty(1.0 / 42, USD) +- 1e-20)
    Qty(1, BBL) / Qty(1, GAL) should be (Qty(42, SCALAR) +- 1e-20)
    Qty(1.0, BBL) / Qty(1.0, GAL) should be (Qty(42.0, SCALAR) +- 1e-2)
    Qty(2.0, SCALAR) * Qty(3.0, PERCENT) should be (Qty(0.06, SCALAR))
    Qty(2.0, PERCENT) * Qty(3.0, SCALAR) should be (Qty(0.06, SCALAR) +- 1e-20)
    Qty(2.0, PERCENT) * Qty(3.0, PERCENT) should be (Qty("0.0006", SCALAR) +- 1e-15)
  }

  test("more mult") {
    Qty(1, USD/G) * Qty(1, MT) shouldEqual Qty(1e6, USD)
    Qty(1, USD/MT) * Qty(1, MT) shouldEqual Qty(1, USD)
  }

  test("power and energy") {
    Qty(1, MW) / Qty(1, MWH) shouldEqual Qty(1, HOUR.invert)

    Qty(1, MW) * Qty(1, HOUR) shouldEqual Qty(1, MWH)
    Qty(1, MW) / Qty(1, HOUR) shouldEqual Qty(1, MW / HOUR)

    Qty(1, MWH) / Qty(1, HOUR) shouldEqual Qty(1, MW)
    Qty(1, THM * MWH) / Qty(1, HOUR) shouldEqual Qty(1, THM * MW)
    Qty(1, THM * MW) / Qty(1, MWH) shouldEqual Qty(1, THM / HOUR)
  }

  test("scalar") {
    Qty(100, SCALAR) / Qty(2, SCALAR) shouldEqual Qty(50, SCALAR)
    Qty(100, BBL) / Qty(2, SCALAR) shouldEqual Qty(50, BBL)
    Qty(100, SCALAR) + Qty(2, SCALAR) shouldEqual Qty(102, SCALAR)
  }

  test("percentage") {
    Qty(100, USD) / Qty(1, PERCENT) shouldEqual Qty(10000, USD)
    Qty(1, PERCENT) * Qty(100, USD) shouldEqual Qty(1, USD)
    Qty(100, USD) * Qty(1, PERCENT) shouldEqual Qty(1, USD)
    Qty(100, USD) * (Qty(1, PERCENT) * Qty(1, PERCENT)) shouldEqual Qty(.01, USD)

    Qty(100, SCALAR) / Qty(1, PERCENT) shouldEqual Qty(10000, SCALAR)
    Qty(100, SCALAR) * Qty(1, PERCENT) shouldEqual Qty(1, SCALAR)

    Qty(10, PERCENT) * Qty(1, SCALAR) shouldEqual Qty(0.1, SCALAR)
    Qty(10, PERCENT) * Qty(10, PERCENT) * Qty(1, SCALAR) shouldEqual Qty(0.01, SCALAR)
    Qty(100, PERCENT) / Qty(1, SCALAR) shouldEqual Qty(1, SCALAR)
    Qty(100, PERCENT) / Qty(2, SCALAR) shouldEqual Qty(0.5, SCALAR)
    (Qty(100, PERCENT) * Qty(100, PERCENT)) / Qty(2, SCALAR) shouldEqual Qty(0.5, SCALAR)

    Qty(100, PERCENT) / Qty(1, PERCENT) shouldEqual Qty(100, SCALAR)

    Qty(100, PERCENT).checkedPercent shouldEqual 1.0
  }

  test("SDQty percentage equality bug test") {
    val q1 = Qty("61.349206", PERCENT).asInstanceOf[SDQty]
    val q2 = Qty("61.349206", PERCENT).asInstanceOf[SDQty]
    q1 == q2 shouldBe(true)
  }

  test("equals and hashcode") {
    Qty(2, USD) shouldEqual Qty(2, USD)
    Qty(2, USD).hashCode() shouldEqual Qty(2, USD).hashCode()
    Qty(2, USD) shouldNot be(Qty(2, BBL))
    Qty(2, USD) shouldNot be(Qty(2.1, USD))
    Qty("2", USD) shouldNot be(Qty(2.1, USD))
    Qty("2", USD) shouldNot be(Qty("2.1", USD))
    Qty("2", USD).hashCode() shouldEqual Qty("2", USD).hashCode()
    Qty(2, USD) shouldNot be(2.0)
    Qty("2", USD) shouldNot be(2.0)
    Qty("0.02", SCALAR) shouldEqual (Qty(4.0, PERCENT) * Qty(50.0, PERCENT))
    Qty("2", PERCENT) shouldNot be(2.0)
    Qty("2", PERCENT) shouldNot be(Qty(2.0, SCALAR))
  }

  test("to string") {
    Qty(2, USD).toString shouldEqual "2.0000 USD"
    Qty(2, K_BBL).toString shouldEqual "2.0000 kbbl"
    Qty(2, USD / BBL).toString shouldEqual "2.0000 USD/bbl"
    Qty(2, USD * USD / BBL).toString shouldEqual "2.0000 USD^2/bbl"
    Qty(-2, USD * USD / (BBL * BBL)).toString shouldEqual "-2.0000 USD^2/bbl^2"
    (Qty(100, PERCENT) * Qty(100, PERCENT)).toString shouldEqual "1.0000"
    (Qty(100, PERCENT) * Qty(1, PERCENT)).toFormattedString(dp = 2, useBrackets = false) shouldEqual ".01"
    (Qty(100, PERCENT) / Qty(1, PERCENT)).toString shouldEqual "100.0000"


    Qty(1.25, USD).toMinimalString shouldEqual "1.25 USD"
    Qty(1.5, USD).toMinimalString shouldEqual "1.5 USD"
    Qty(1.0, USD).toMinimalString shouldEqual "1 USD"
  }
  
  test("from string") {
    val validQtys = Seq(Qty(100, UOM.K_BBL), Qty.NULL, Qty(1, SCALAR), Qty(100, USD/BBL), Qty(1, PERCENT))
    val invalidQtys = Seq("", " ", "Invalid", "1 bb", "bbl", "-75.   232 Lot")

    validQtys.foreach {
      q =>
        Qty.unapply(q.toString()) shouldEqual Some(q)
    }
    invalidQtys.foreach {
      q =>
        Qty.unapply(q) shouldEqual None
        Qty.fromString(q) shouldBe 'left
    }

    Qty.fromString("1,000USD") shouldBe Right(1000 (USD))
    Qty.fromString("1e3USD") shouldBe Right(1000 (USD))
    Qty.fromString("1,000 USD") shouldBe Right(1000 (USD))
    Qty.fromString("1,000.50 USD") shouldBe Right(Qty(1000.5, USD))
    
    Qty.fromString("100.") shouldBe 'left
    Qty.fromString("100..") shouldBe 'left
    Qty.fromString(".1") shouldBe 'right
    Qty.fromString(".1 BBL") shouldBe 'right
  }

  test("format") {
    Qty(2, USD).toFormattedString(2, useBrackets = false) shouldEqual "2.00 USD"
    Qty(2, USD).toFormattedString(0, useBrackets = false) shouldEqual "2 USD"
    Qty(1.99, USD).toFormattedString(2, useBrackets = false) shouldEqual "1.99 USD"
    
    Qty(1.99121, USD).toFormattedString("#.00##", useBrackets = false) shouldEqual "1.9912 USD"
    Qty(1.991, USD).toFormattedString("#.00##", useBrackets = false) shouldEqual "1.991 USD"
    Qty(-2, USD).toFormattedString("#.00##", useBrackets = true) shouldEqual "(2.00) USD"

    // this test fails in Java 8. it gives 1.9 USD
    // https://github.com/PROSPricing/jdk8patch-halfupround/
    // Fixed in JDK 1.8.0_40-b25
    Qty("1.99", USD).toFormattedString(1, useBrackets = false) shouldEqual "2.0 USD"

    Qty(1.99, USD).toFormattedString(0, useBrackets = false) shouldEqual "2 USD"
//    decimal format gives up after 16 decimal places.
//    so after 16 decimal places we just print out the string for the bigdecimal and the uom.
    (Qty("1", USD) - Qty("1e-20", USD)).toFormattedString(300, useBrackets = false) shouldEqual "0.99999999999999999999 USD"
  }

  test("implicits") {
    BigDecimal("2").ensuringFixedPoint shouldEqual Qty("2", SCALAR)
    BigDecimal(2).ensuringFixedPoint shouldEqual Qty("2", SCALAR)
    Qty(4, USD) * 4 shouldEqual Qty(16, USD)

    4(USD) shouldEqual Qty(4, USD)
  }

  test("conversion") {
    1(USD) in US_CENT shouldEqual Right(100(US_CENT))
    (1(USD) in US_CENT).flatMap(_ in USD) shouldEqual Right(1(USD))
    100(US_CENT) in USD shouldEqual Right(1(USD))
    1(BBL) in GAL shouldEqual Right(42(GAL))
    1(USD/G) in USD/KG shouldEqual Right(1000(USD/KG))

    val conv1 = DimensionConversions.fromMap(Map((MT, BBL) -> 7.45)).get
    Qty("1", USD / BBL).in(USD / MT, conv1).R.round(30) shouldEqual Qty("7.45", USD / MT)
    Qty("1", SCALAR) in (USD/MT, conv1) should be ('left)

    1(BBL) in (BBL/MT, conv1) should be ('left)
         
    1(MT) in BBL should be('left)
    1(MT).in(BBL, conv1) shouldEqual Right(Qty(7.45, BBL))
    Qty("1", USD / BBL).in(USD / MT, conv1).R.round(30) shouldEqual Qty("7.45", USD / MT)
    Qty("100", USD / BBL).in(USD / MT, conv1).R.round(30) shouldEqual 745(USD / MT)
    Qty("745", USD / MT).in(USD / BBL, conv1).R.round(30) shouldEqual Qty("100", USD / BBL)
    Qty("7.45", USD / MT).in(USD / BBL, conv1).R.round(30) shouldEqual Qty("1", USD / BBL)

    val conv2 = DimensionConversions.fromMap(Map((G, BBL) -> 7.45 / 1e6)).get
    1(MT).in(BBL, conv2) shouldEqual Right(Qty(7.45, BBL))
    1000(KG/M3) in (G/CM3) shouldEqual Right(1(G/CM3))

    // GAL -> BBL -> MT -> KG
    //   1 / 42  /  7.45 * 1000

    1(GAL).in(KG, conv1).R should be ((Qty(1, SCALAR) / 42) / 7.45 * 1000(KG) +- 1e-9)

    (Qty("15124.766960", US_CENT) in USD).R should be (Qty("151.247670", USD) +- 1e-6)
  }

  test("more complex conversions") {
    // all the same conversion expressed with different UOMs of the same dimension
    val conv1 = DimensionConversions.fromMap(Map((G, BBL) -> 10)).get
    val conv2 = DimensionConversions.fromMap(Map((KG, BBL) -> 10000)).get
    val conv3 = DimensionConversions.fromMap(Map((KG, GAL) -> 42 * 10000)).get
    val conv4 = DimensionConversions.fromMap(Map((G, GAL) -> 420)).get
    val conv5 = DimensionConversions.fromMap(Map((G, GAL) -> 420)).get
    val conv6 = DimensionConversions.fromMap(Map((BBL, G) -> 0.1)).get
    val conv7 = DimensionConversions.fromMap(Map((GAL, G) -> BigDecimal(0.1)/42)).get

    for((convA, convB) <- List(conv1, conv2, conv3, conv4, conv5, conv6, conv7).cartesian) {
      1(GAL).in(KG, convA).R shouldEqual 1(GAL).in(BBL).R.in(MT, convB).R.in(KG).R
      1(KG).in(BBL, convA) shouldEqual Right(10000(BBL))
      1(KG).in(BBL, convB) shouldEqual Right(10000(BBL))
      1(MT).in(BBL, convB) shouldEqual 1(MT).in(BBL, convA)
      1(MT).in(GAL, convA).R shouldEqual 1(MT).in(BBL, convA).R.in(GAL).R
      1(MT).in(GAL, convB).R shouldEqual 1(MT).in(BBL, conv3).R.in(GAL).R
      1(G).in(GAL, convA).R.in(MT, convB).R shouldEqual 1(G).in(MT).R
      1(G).in(GAL, convA).R.in(MT, convB).R.in(BBL, convA) shouldEqual 1(G).in(GAL, convB).R.in(BBL)
    }
  }

  test("conversions with powers") {
    val conv1 = DimensionConversions.fromMap(Map((G, BBL) -> 1)).get

    1(GAL.pow(2)).in(BBL.pow(2)).R.bdValue shouldEqual BigDecimal(1) / 42 / 42

    1(KG.pow(2)).in(MT.pow(2)).R.in(G.pow(2)).R.in(KG.pow(2)).R should be (1(KG.pow(2)) +- 1e-20)
    1(BBL.pow(2)).in(GAL.pow(2)).R.in(BBL.pow(2)).R should be (1(BBL.pow(2)) +- 1e-20)

    1(G.pow(2)).in(BBL.pow(2), conv1).R should be (1(G.pow(2)).in(GAL.pow(2), conv1).R.in(BBL.pow(2)).R +- 1e-20)

    val conv2 = DimensionConversions.fromMap(Map((MT, BBL) -> 7.45)).get
    1(MT.pow(2)).in(BBL.pow(2), conv2) shouldEqual Right(Qty("7.45", BBL) * Qty("7.45", BBL))
    (Qty("7.45", BBL) * Qty("7.45", BBL)).in(MT.pow(2), conv2).R.round(30) shouldEqual 1(MT*MT)

    val bd = BigDecimal(7.45) * BigDecimal(7.45)
    Qty(bd, USD/MT.pow(2)).in(USD/BBL.pow(2), conv2).R.round(30) shouldEqual 1(USD/BBL.pow(2))
  }

  test("conversions that chain") {
    val conv1 = DimensionConversions.fromMap(Map((G, BBL) -> 5, (BBL, MMBTU) -> 7)).get

    1(G).in(MMBTU, conv1).get shouldEqual Qty(35, MMBTU)
    1(G.pow(2)).in(MMBTU.pow(2), conv1).get shouldEqual Qty(35*35, MMBTU.pow(2))

    35(MMBTU).in(G, conv1).get shouldEqual Qty(1, G)
    (35*35)(MMBTU.pow(2)).in(G.pow(2), conv1).get should be (Qty(1, G.pow(2)) +- 1e-2)
  }

  test("conversions that chain with inversion") {
    val conv1 = DimensionConversions.fromMap(Map((G, BBL) -> 5, (MMBTU -> BBL) -> .25)).get

    1(G).in(MMBTU, conv1).get shouldEqual Qty(20, MMBTU)
    1(G.pow(2)).in(MMBTU.pow(2), conv1).get shouldEqual Qty(400, MMBTU.pow(2))

    20(MMBTU).in(G, conv1).get shouldEqual Qty(1, G)
    400(MMBTU.pow(2)).in(G.pow(2), conv1).get should be (Qty(1, G.pow(2)) +- 1e-2)
  }

  test("conversions that should fail") {
    1000(KG/M3) in (G/CM) shouldBe 'left
    1000(KG/M) in CM shouldBe 'left
    1000(KG/M) in G shouldBe 'left
    1000(KG/M) in KG shouldBe 'left
    1000(KG) in CM shouldBe 'left
    1000(M3) in M shouldBe 'left

    val conv1 = DimensionConversions.fromMap(Map((G, BBL) -> 1)).get
    // We could make this work if we needed it, but for now it doesn't
    1000(G*MT).in(GAL*BBL, conv1) shouldBe 'left
  }

  test("abs") {
    1(USD).abs shouldEqual 1(USD)
    Qty("-1", USD).abs shouldEqual 1(USD)
  }

  test("average") {
    intercept[RuntimeException] {
      Qty.average(Nil)
    }
    Qty.average(1(USD) :: Nil) shouldEqual 1(USD)
    Qty.average(1(USD) :: 2(USD) :: Nil) shouldEqual Qty(1.5, USD)
    intercept[RuntimeException] {
      Qty.average(1(USD) :: 2(BBL) :: Nil)
    }
  }
  
  test("percent/scalar behaviour") {
    40(PERCENT) / 2(SCALAR) shouldEqual 20(PERCENT)
    40(PERCENT) / 2(SCALAR) shouldEqual Qty("0.2", SCALAR)
    40 (PERCENT) * 50 (PERCENT) shouldEqual 20 (PERCENT)
    (40(PERCENT) / 50(PERCENT)) shouldEqual (40 (PERCENT) * (50 (PERCENT)).invert)
    (40(PERCENT) / 50(PERCENT)).asPercent shouldEqual (40 (PERCENT) * (50 (PERCENT)).invert).asPercent
    
    40(PERCENT).asPercent shouldEqual Qty("0.4", SCALAR)
    40(PERCENT) shouldEqual Qty("0.4", SCALAR)
    40(PERCENT).asPercent shouldEqual Qty("0.4", SCALAR).asPercent
    40(PERCENT) shouldEqual Qty("0.4", SCALAR).asPercent
    
    40(PERCENT).asPercent.checkedPercent shouldEqual 0.4
    Qty("0.4", SCALAR).asPercent.checkedPercent shouldEqual 0.4
    40(PERCENT).asPercent.uom shouldEqual PERCENT
    Qty("0.4", SCALAR).asPercent.uom shouldEqual PERCENT
    Qty("0.4", SCALAR).asPercent.bdValue shouldEqual BigDecimal(40)

    Qty("0.4", SCALAR).asPercent.toString shouldEqual "40.0000 %"
    40(PERCENT).toString shouldEqual "40.0000 %"
    40(PERCENT).asPercent.toString shouldEqual "40.0000 %"
  }

  test("compare") {
    import Qty._
    (1(USD) < Qty(2.0, USD)) shouldBe true
    (2(USD) > 1(USD)) shouldBe true
    List(1(SCALAR), Qty.NULL, 0(BBL)).foreach{
      q => 
        intercept[RuntimeException] {
          2(USD) > q
        }
    }
    intercept[RuntimeException] {
      2(USD) > 1
    }

    (2(USD) > 0) shouldBe true
    (2(USD) >= 0) shouldBe true
    (Qty(0.0, USD) >= 0) shouldBe true
    (Qty(2, USD) < 0) shouldBe false
  }

  test("fixed types add/subtract") {
    assert(Qty("1", USD).isInstanceOf[SDQty])
    assert((Qty("1.", USD) + Qty(".0000005", USD)).isInstanceOf[BDQty])
    assert((Qty("1.", USD) + Qty(".00005", US_CENT)).isInstanceOf[BDQty])
    assert((Qty("1.", USD) + Qty(".5", US_CENT)).isInstanceOf[SDQty])
    assert((Qty("1.", USD) + Qty(".5", USD)).isInstanceOf[SDQty])

    assert((Qty("1.", USD) + Qty.NULL).isInstanceOf[SDQty])
    assert((Qty.NULL + Qty(".5", USD)).isInstanceOf[SDQty])

    assert(Qty("9223372036854", USD).isInstanceOf[SDQty]) // pretty much max value for SDQty
    assert((Qty("9223372036854", USD) + Qty("9223372036854", USD)).isInstanceOf[BDQty])

    assert(Qty("1.0000005", USD).isInstanceOf[BDQty])
    assert((Qty("1.0000005", USD) - Qty(".0000005", USD)).isInstanceOf[SDQty])
    assert((Qty("1.0000005", USD) - Qty(".00005", US_CENT)).isInstanceOf[SDQty])
  }

  test("fixed types mult/div") {
    assert(Qty("9223372036854", USD).isInstanceOf[SDQty]) // pretty much max value for SDQty
    assert((Qty("9223372036854", USD) * 1).isInstanceOf[SDQty])
    assert((Qty("9223372036854", USD) * 2).isInstanceOf[BDQty])
    assert(((Qty("9223372036854", USD) * 2) / 2).isInstanceOf[SDQty])
    assert((Qty("9223372036854", USD) * Qty("0.2", SCALAR)).isInstanceOf[SDQty])
    assert(((Qty("9223372036854", USD) * Qty(1, PERCENT)) * Qty(10000, PERCENT)).isInstanceOf[SDQty])

    assert((Qty("9223372036854", USD) / 2).isInstanceOf[SDQty])
    assert((Qty("9223372036854", USD) / Qty("0.5", SCALAR)).isInstanceOf[BDQty])
    assert((Qty("9223372036854", USD) / Qty(1, PERCENT)).isInstanceOf[BDQty])
    assert((Qty("1", USD) / 3).isInstanceOf[BDQty])
    assert((Qty("1", USD) / 2).isInstanceOf[SDQty])
  }

  test("test SDQty arithmetic") {
    assert((Qty("123", THM) / Qty("30", DAY)) === Qty("4.1", THM/DAY))

    assert((Qty("1.", USD) + Qty(".5", USD)).bdValue === BigDecimal("1.5"))
    assert((Qty("1.", USD) + Qty("50", US_CENT)).bdValue === BigDecimal("1.5"))
    assert((Qty("1.", USD) + Qty("0.0000005", USD)).bdValue === BigDecimal("1.0000005"))
    assert((Qty("1.0000005", USD) - Qty(".0000005", USD)).bdValue === BigDecimal("1"))
    assert((Qty("1.0000005", USD) - Qty(".00005", US_CENT)).bdValue === BigDecimal("1"))

    assert((Qty("10", USD) / 3).bdValue === (BigDecimal("10")/BigDecimal("3")))
    assert((Qty("10", USD) / 2).bdValue === (BigDecimal("10")/BigDecimal("2")))
    assert((Qty("5", USD) * Qty("0.2", SCALAR)).bdValue === BigDecimal("1"))
    assert((Qty("5.55", USD) * Qty("0.12345", SCALAR)).bdValue === BigDecimal("5.55")*BigDecimal("0.12345"))
    assert((Qty("5.55", USD) * Qty("0.1234567", PERCENT)).bdValue === BigDecimal("5.55")*BigDecimal("0.001234567"))
    assert((Qty("5", USD) * (Qty(1, USD) / Qty(3, US_CENT))).bdValue ===
      BigDecimal("5")*(BigDecimal("1")/BigDecimal("0.03")))

    assert((Qty("5", USD) / Qty("0.2", SCALAR)).bdValue === BigDecimal("25"))
    assert((Qty("3", USD) * Qty(1, PERCENT)).bdValue === BigDecimal("0.03"))
    assert((Qty("3", USD) / Qty(1, PERCENT)).bdValue === BigDecimal("300"))
    assert((Qty("3", USD) / (Qty(1, USD) / Qty(3, US_CENT))).bdValue ===
      (BigDecimal("3")/(BigDecimal("1")/BigDecimal("0.03"))))
  }

  test("rounding") {
    Qty(1.2, SCALAR).round(0) shouldEqual Qty(1, SCALAR)
    Qty(1.25, SCALAR).round(1) shouldEqual Qty(1.3, SCALAR)
    Qty(12345, BBL).round(-1) shouldEqual Qty(12350, BBL)
    Qty(12345, BBL).round(-3) shouldEqual Qty(12000, BBL)
  }

  test("significant figures") {
    Qty(1.23456, SCALAR).significantFigures(2) shouldEqual Qty(1.2, SCALAR)
    Qty(1.23456, SCALAR).significantFigures(3) shouldEqual Qty(1.23, SCALAR)
    Qty(123456, SCALAR).significantFigures(2) shouldEqual Qty(120000, SCALAR)
    Qty(123456, SCALAR).significantFigures(3) shouldEqual Qty(123000, SCALAR)
    Qty(54321, BBL).significantFigures(2) shouldEqual Qty(54000, BBL)
  }

  test("some other funcs") {
    Qty.NULL.isNull shouldBe true
    Qty("5", USD).invert shouldEqual Qty("0.2", USD.invert)
    assert(Qty("5", USD).invert.isInstanceOf[SDQty])
    Qty("3", USD).invert shouldEqual Qty(BigDecimal(1)/3, USD.invert)
    assert(Qty("3", USD).invert.isInstanceOf[BDQty])

    Qty(300.0, US_CENT).inBaseCcy shouldEqual Qty("3", USD)
    Qty("3", USD).inBaseCcy shouldEqual Qty("3", USD)
    Qty("300", US_CENT).inBaseCcy shouldEqual Qty("3", USD)

    Qty(0, SCALAR) <= 0 shouldBe true
    Qty.NULL <= 0 shouldBe true
  }
  
  test("toFixedQty") {
    Qty("1", SCALAR).toFixedQty() shouldBe a [SDQty]
    Qty("1.1234567890", SCALAR).toFixedQty() shouldBe a [SDQty]
    Qty("-1.1234567890", SCALAR).toFixedQty() shouldBe a [SDQty]
    Qty("123456789101112131415", SCALAR).toFixedQty() shouldBe a [BDQty]
    Qty("-123456789101112131415", SCALAR).toFixedQty() shouldBe a [BDQty]
  }
  
  test("toSDQty") {
    Qty("1", SCALAR).toSDQty.right.value shouldBe a [SDQty]
    Qty("1.1234567890", SCALAR).toSDQty.right.value shouldBe a [SDQty]
    Qty("-1.1234567890", SCALAR).toSDQty.right.value shouldBe a [SDQty]
    Qty("123456789101112131415", SCALAR).toSDQty shouldBe 'left
    Qty("-123456789101112131415", SCALAR).toSDQty shouldBe 'left
  }

  test("Named quantities tree") {
    implicit val naming = NamingEnabled(true)

    val prices = (1 to 5).map {
      i =>
        Qty(i, USD / BBL).named(s"d${i}Price")
    }

    val F = Qty.average(prices).named("F")
    val K = 10 (USD / BBL).named("K")
    val Q = 100 (BBL).named("Q")
    val disc = Qty("0.98", SCALAR).named("disc")

    val value = ((F - K) * Q * disc).named("Value").asInstanceOf[NamedQty]

    value.tree.multilineString(lookAtShouldExpandFlag = false) should equal(
      """
        |Value = -686 USD
        |(((F - K) * Q) * disc) = -686 USD
        |((F - K) * Q) = -700 USD
        |disc = 0.98
        |(F - K) = -7 USD/bbl
        |Q = 100 bbl
        |F = 3 USD/bbl
        |K = 10 USD/bbl
        |Average(5 values) = 3 USD/bbl
        |d1Price = 1 USD/bbl
        |d2Price = 2 USD/bbl
        |d3Price = 3 USD/bbl
        |d4Price = 4 USD/bbl
        |d5Price = 5 USD/bbl
        """.stripMargin
    ) (after being whiteSpaceNormalised)
  }
  
  test("named qty min and max") {
    implicit val naming = NamingEnabled(true)
    val max = Qty.max(Seq(100 (BBL).named("A"), 120 (BBL).named("B")))
    max.asInstanceOf[NamedQty].tree.multilineString(lookAtShouldExpandFlag = true) should equal (
      """Max(A, B) = 120 bbl"""
    ) (after being whiteSpaceNormalised)
    val min = Qty.min(Seq(100 (BBL).named("A"), 120 (BBL).named("B")))
    min.asInstanceOf[NamedQty].tree.multilineString(lookAtShouldExpandFlag = true) should equal (
      """Min(A, B) = 100 bbl"""
    ) (after being whiteSpaceNormalised)
  }

  test("Named quantities expand flag") {
    implicit val naming = NamingEnabled(true)

    val prices = (1 to 5).map {
      i =>
        Qty(i, USD / BBL).named(s"d${i}Price")
    }

    val F = Qty.average(prices).named("F")
    val K = 10 (USD / BBL).named("K")
    val Q = 100 (BBL).named("Q")
    val disc = Qty("0.98", SCALAR).named("disc")

    val value = (((F - K).interesting * Q) * disc).named("Value").asInstanceOf[NamedQty]

    value.tree.multilineString(lookAtShouldExpandFlag = true) should equal(
      """
        |Value = -686 USD
        |(((F - K) * Q) * disc) = -686 USD
        |((F - K) * Q) = -700 USD
        |disc = 0.98
        |(F - K) = -7 USD/bbl
        |Q = 100 bbl
        |F = 3 USD/bbl
        |K = 10 USD/bbl
        """.stripMargin
    ) (after being whiteSpaceNormalised)
  }

  test("named quantities") {
    val a = 100 (USD).asNamed("a")
    val b = 200 (USD).asNamed("b")
    a + b shouldEqual BinaryOpNamedQty("+", a, b, 300(USD))
    a - b shouldEqual BinaryOpNamedQty("-", a, b, -100(USD))
    a * b shouldEqual BinaryOpNamedQty("*", a, b, 20000(USD*USD))
    a / b shouldEqual BinaryOpNamedQty("/", a, b, Qty("0.5", SCALAR))
    (a + b) / 2 shouldBe FunctionNamedQty("Average", renderInline = false, Seq(a, b), Qty(150, USD))
    (a + b + a) shouldBe FunctionNamedQty("Sum", renderInline = false, Seq(a, b, a), Qty(400, USD))
    (a + a + a) / 3 shouldBe FunctionNamedQty("Average", renderInline = false, Seq(a, a, a), Qty(100, USD))
    
    val c = a.toFixedQty().asInstanceOf[NamedQtyFixed]
    val d = b.toFixedQty().asInstanceOf[NamedQtyFixed]
    c.plus(d) shouldBe BinaryOpFixedNamedQty("+", c, d, 300(USD))
    c.minus(d) shouldEqual BinaryOpFixedNamedQty("-", c, d, -100(USD))
    c.mult(d) shouldEqual BinaryOpFixedNamedQty("*", c, d, 20000(USD*USD))
    c.div(d) shouldEqual BinaryOpFixedNamedQty("/", c, d, Qty("0.5", SCALAR).toFixedQty())

    c.plus(d).div(2) shouldBe FunctionFixedNamedQty("Average", renderInline = true, Seq(c, d), Qty(150, USD))
    c.plus(d).plus(c) shouldBe FunctionFixedNamedQty("Sum", renderInline = false, Seq(c, d, c), Qty(400, USD))
    c.plus(c).plus(c).div(3) shouldBe FunctionFixedNamedQty("Average", renderInline = false, Seq(c, c, c), Qty(100, USD))
  }
  
  test("named quantities text") {
    val a = 100 (USD).asNamed("a")
    val b = 200 (USD).asNamed("b")
    
    val c = a + b - a * (-a) / a + a.abs + b.invert.invert
    c.asNamed().name shouldEqual "Sum(3 values)"
    c.asNamed().tree.multilineString(lookAtShouldExpandFlag = false) should equal(
      """
        |Sum(3 values) = 700 USD
        |((a + b) - ((a * -a) / a)) = 400 USD
        |Abs(a) = 100 USD
        |(1/(1/b)) = 200 USD
        |(a + b) = 300 USD
        |((a * -a) / a) = -100 USD
        |a = 100 USD
        |b = 200 USD
        |(a * -a) = -10,000 USD^2
        |a = 100 USD
        |a = 100 USD
        |-a = -100 USD
        |a = 100 USD
        |a = 100 USD
        |(1/b) = 0.005 USD^-1
        |b = 200 USD""".stripMargin
    ) (after being whiteSpaceNormalised)

    c.asNamed().qty shouldEqual ((a + b) - ((a * -a) / a)) + a + (1/(1/b))
  }

  test("taints") {
    Qty(10, BBL).taint shouldBe Taint.clean
    Qty(10, BBL).withTaint(Taint.marketData).taint shouldBe Taint.marketData
    Qty(10, BBL, Taint.marketData).taint shouldBe Taint.marketData
    (Qty(10, BBL) + Qty(10, BBL, Taint.marketData)).taint shouldBe Taint.marketData

    val q1s = Seq(
      Qty(10, BBL, Taint.marketData),
      Qty(10.0, BBL, Taint.marketData),
      Qty(BigDecimal(1e20), BBL, Taint.marketData)
    )

    val q2s = Seq(
      Qty(10, BBL, Taint.tradeData),
      Qty(10.0, BBL, Taint.tradeData),
      Qty(BigDecimal(1e20), BBL, Taint.tradeData)
    )

    for (q1 <- q1s; q2 <- q2s) {
      def test(q: Qty, expected: Taint) = q.taint shouldEqual expected

      val expected1_2 = Taint.marketData | Taint.tradeData
      test(q1 * q2, expected1_2)
      test(q1 - q2, expected1_2)
      test(q1 / q2, expected1_2)
      test(q1 + q2, expected1_2)

      val expected1 = Taint.marketData
      test(q1.negate, expected1)
      test(q1.abs, expected1)
      test(q1.dblQty, expected1)
      test(q1.invert, expected1)
      test(q1.unary_-, expected1)
      test(q1.zero, expected1)
    }

  }
}
