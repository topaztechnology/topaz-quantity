package com.topaz.quantity

import com.topaz.quantity.ConversionTree.ConversionTreeBuilder
import com.topaz.quantity.UOM.{SingleUOM, _}
import com.topaz.utils.{GeneralTopazFail, TopazFail}
import org.scalatest.tagobjects.Slow

import scala.math.BigDecimal
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DimensionConversionsTest extends AnyFunSuite with Matchers {

  /**
   * Similar to the conversion code in MapQtyConversions but maybe a bit easier to read.
   * Used to test against the MapQtyConversions code.
   * Can invert twice which leads to rounding errors, so that's why it's not used in the main code.
   */
  private def rate(conv: (SingleUOM, SingleUOM) => Option[BigDecimal],
                   from: SingleUOM, to: SingleUOM): Option[BigDecimal] = {
    if (from.power == to.power) {
      val fromBase = from.base
      val toBase = to.base

      conv(fromBase, toBase) match {
        case Some(conv) =>
          val rescale = to.conversionToBase / from.conversionToBase
          Some((conv / rescale).pow(from.power))
        case _ =>
          conv(toBase, fromBase) match {
            case Some(conv) =>
              val rescale = from.conversionToBase / to.conversionToBase
              Some((rescale / conv).pow(from.power))
            case _ => None
          }
      }
    } else {
      None
    }
  }

  import scala.language.implicitConversions
  implicit def singleUOM(uom: UOM): SingleUOM = uom.asSingleUOM

  test("simple conversions", Slow) {
    val conv = DimensionConversions.fromMap(Map((MT -> BBL) -> 7.5)).get
    val dp = BigDecimal("1e-20")

    conv.rate(MT, BBL) shouldEqual Right(BigDecimal(7.5))
    conv.rate(MT.pow(2), BBL.pow(2)) shouldEqual Right(BigDecimal(7.5) * 7.5)
    conv.rate(BBL, MT) shouldEqual Right(1 / BigDecimal(7.5))
    conv.rate(G, BBL) shouldEqual Right(BigDecimal(7.5) / 1e6)
    conv.rate(BBL, G) shouldEqual Right(1e6 / BigDecimal(7.5))
    conv.rate(GAL, G).get shouldEqual 1e6 / (BigDecimal(7.5) * 42) +- dp

    conv.rate(MT.pow(-1), BBL.pow(-1)) shouldEqual Right(1 / BigDecimal(7.5))
    conv.rate(MT.pow(-2), BBL.pow(-2)).get shouldEqual 1 / (BigDecimal(7.5) * BigDecimal(7.5)) +- dp
    conv.rate(BBL.pow(-1), MT.pow(-1)) shouldEqual Right(BigDecimal(7.5))
    conv.rate(G.pow(-1), BBL.pow(-1)) shouldEqual Right(1e6 / BigDecimal(7.5))
    conv.rate(BBL.pow(-1), G.pow(-1)) shouldEqual Right(BigDecimal(7.5) / 1e6)
    conv.rate(GAL.pow(-1), G.pow(-1)) shouldEqual Right((BigDecimal(7.5) * 42) / 1e6)
  }

  test("conversions against different algorithms", Slow) {
    val tol = BigDecimal("1e20")

    val conversions = List(
      DimensionConversions.fromMap(Map((G, BBL) -> 2, (MMBTU -> GAL) -> 10, (US_CENT -> KG) -> 5)).get,
      DimensionConversions.fromMap(Map((MT -> BBL) -> 7.5, (MMBTU -> BBL) -> 3, (USD -> BBL) -> 4)).get
    )

    for(conv <- conversions) {
      // using the underlying DimensionConversions to find the base conversion rate
      def convert1(from: SingleUOM, to: SingleUOM) =
        rate((f, t) => conv.rate(f, t).toOption, from, to)

      // using the ConversionTree to find the base conversion rate
      val tree = {
        val b = new ConversionTreeBuilder[SingleUOM]()
        conv.baseConversionRates.foreach {
          case ((from, to), Right(rate)) => b.add(from, to, rate)
          case _ =>
        }
        b.build
      }
      def convert2(from: SingleUOM, to: SingleUOM) =
        rate((f, t) => tree.conversion(f, t), from, to)

      val uoms = List(BBL, MT, GAL, G, KG, MMBTU, THM, USD, US_CENT)
      val powers = (-3).to(3).filterNot(_ == 0)
      for (pow1 <- powers; uom1 <- uoms; pow2 <- powers; uom2 <- uoms) {
        val from = uom1.pow(pow1)
        val to = uom2.pow(pow2)
        if (pow1 == pow2 && from.base != to.base) {
          val dp = conv.rate(from, to).get / tol
          conv.rate(from, to).get shouldEqual convert1(from, to).get +- dp
          conv.rate(from, to).get shouldEqual convert2(from, to).get +- dp
        } else {
          conv.rate(from, to) shouldBe 'left
          convert1(from, to) shouldBe empty
          convert2(from, to) shouldBe empty
        }
      }
    }
  }

  test("should fail when there's a cycle") {
    DimensionConversions.fromMap(Map(
      (G -> BBL) -> 2,
      (BBL -> THM) -> 3,
      (USD -> THM) -> 4,
      (G -> USD) -> 5
    )) shouldBe 'left
    DimensionConversions.fromMap(Map(
      (G -> BBL) -> 2,
      (BBL -> THM) -> 3,
      (G -> THM) -> 4
    )) shouldBe 'left
    DimensionConversions.fromMap(Map(
      (G -> BBL) -> 2,
      (BBL -> THM) -> 3,
      (G -> THM) -> 4
    )) shouldBe 'left
  }

  test("Should fail when provided multiple conversions for the same base") {
    DimensionConversions.fromMap(Map(
      (G -> BBL) -> 2,
      (MT -> BBL) -> 3
    )) shouldBe 'left
  }

  test("Should fail when provided a conversion for uoms of the same base") {
    DimensionConversions.fromMap(Map(
      (G -> MT) -> 2
    )) shouldBe 'left
  }

  test("Should parse strings containing valid semicolon-delimited conversions") {
    val expected1 = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.7"),
      (M3, BBL) -> BigDecimal("6")
    )).get
    val expected2 = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.789"),
      (M3, BBL) -> BigDecimal("5.9964")
    )).get
    DimensionConversions.fromString("") shouldBe Right(DimensionConversions.empty)
    DimensionConversions.fromString(" ") shouldBe Right(DimensionConversions.empty)
    DimensionConversions.fromString(expected1.toPersistentString) shouldBe Right(expected1)
    DimensionConversions.fromString(expected2.toPersistentString) shouldBe Right(expected2)
    DimensionConversions.fromString("1m3=0.7MT;1m3=6bbl") shouldBe Right(expected1)
    DimensionConversions.fromString("1 m3=0.7 MT; 1 m3=6 bbl") shouldBe Right(expected1)
    DimensionConversions.fromString("1 m3=0.789 MT; 1 m3=5.9964 bbl") shouldBe Right(expected2)
    DimensionConversions.fromString("1 m3 = 0.789 MT; 1 m3 = 5.9964 bbl") shouldBe Right(expected2)
    DimensionConversions.fromString("1  m3  =  0.789  MT;  1  m3  =  5.9964  bbl") shouldBe Right(expected2)
  }

  test("Should fail to parse strings containing invalid conversions") {
    def fails(string: String) = DimensionConversions.fromString(string) shouldBe 'left

    fails("1")
    fails("1 m3")
    fails("1 m3=")
    fails("1 m3=6")
    fails("1 m3=6.")
    fails("1 m3=6.1")
    fails("1 m3=6.1 ")
    fails("1 m3=6.1 b")
    fails("1 m3=6.1 bb")
    fails("2 m3 = 0.789 MT")
    fails("1 invalidUOM = 0.789 MT")
    fails("1 m3=0 MT")
    fails("1 invalidUOM=6.1 bb; 2 MT=15 bb")
  }

  test("should combine DimensionConversions") {
    val a = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.7"),
      (M3, BBL) -> BigDecimal("6")
    )).get
    val b = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.7"),
      (M3, BBL) -> BigDecimal("6")
    )).get
    val c = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.8"),
      (M3, BBL) -> BigDecimal("6")
    )).get
    val d = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.8"),
    )).get
    val e = DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.9"),
    )).get
    val f = DimensionConversions.fromMap(Map(
      (M3, KG) -> BigDecimal("700"),
    )).get

    DimensionConversions.commonConversions(Nil) shouldBe DimensionConversions.empty
    DimensionConversions.commonConversions(Seq(a)) shouldBe a
    
    DimensionConversions.commonConversions(Seq(a, b)) shouldBe a
    
    DimensionConversions.commonConversions(Seq(a, c)) shouldBe DimensionConversions.fromMap(Map(
      (M3, BBL) -> BigDecimal("6")
    )).get
    
    DimensionConversions.commonConversions(Seq(c, d)) shouldBe DimensionConversions.fromMap(Map(
      (M3, MT) -> BigDecimal("0.8")
    )).get
    
    DimensionConversions.commonConversions(Seq(d, e)) shouldBe DimensionConversions.empty
    
    DimensionConversions.commonConversions(Seq(a, f)) shouldBe f
  }
  
}
