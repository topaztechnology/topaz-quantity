package com.topaz.quantity

import java.io._

import com.topaz.quantity.UOM._
import com.topaz.utils.{EitherTestPimps, GeneralTopazFail, TopazFail}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.tagobjects.Slow

import scala.math.BigDecimal
import scala.util.Left
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UOMTest extends AnyFunSuite with Matchers with EitherTestPimps 
  with TableDrivenPropertyChecks
{

  test("addition/subtraction", Slow) {
    def test(uom1: UOM, uom2: UOM) = uom1.addOrSubtract(uom2)
    test(USD, USD) shouldEqual Right(None)
    test(USD, US_CENT) shouldEqual Right(Some(BigDecimal(100)))
    test(US_CENT, USD) shouldEqual Right(Some(BigDecimal(.01)))
    test(US_CENT, BBL) shouldEqual TopazFail("Can't add [¢] and [bbl]")
    test(BBL, USD) shouldEqual TopazFail("Can't add [bbl] and [USD]")
  }

  test("simple multiplication") {
    (USD * USD * BBL).div(US_CENT * GAL) match {
      case (USD, bd) => bd shouldEqual 100 * BigDecimal(42)
      case _ => fail()
    }
    USD.mult(BBL).toString shouldEqual "(bbl USD,1)" // the order of the string bbl USD is not stable if you add/remove UOMs
    USD.mult(USD).toString shouldEqual "(USD^2,1)"
    USD.div(US_CENT).toString shouldEqual "(,1.0E+2)"
    USD.mult(USD)._1.div(US_CENT).toString shouldEqual "(USD,1.0E+2)"
  }

  test("reduce and intern") {
    (USD / MT).mult(MT) match {
      case (USD, bd) => bd shouldEqual BigDecimal(1.0)
      case _ => fail()
    }
  }

  test("toString") {
    NULL.toString shouldEqual "NULL"
    SCALAR.toString shouldEqual ""
    SHARE.toString shouldEqual "share"
    USD.toString shouldEqual "USD"
    (USD * USD).toString shouldEqual "USD^2"
    (BBL / (USD * USD)).toString shouldEqual "bbl/USD^2"
    (SCALAR / USD).toString shouldEqual "USD^-1"
    (SCALAR / (USD * USD)).toString shouldEqual "USD^-2"
    (USD / SCALAR).toString shouldEqual "USD"
    (USD / SHARE).toString shouldEqual "USD/share"
  }

  test("fromName") {
    UOM.fromName("USD") shouldEqual Some(USD)
    UOM.fromName("USC") shouldEqual Some(US_CENT)
    UOM.fromName("US_CENT") shouldEqual Some(US_CENT)
    UOM.fromName("USD1") shouldEqual None
  }

  test("Scalar category") {
    SCALAR.isScalarCategory shouldBe (true)
    PERCENT.isScalarCategory shouldBe (true)
    USD.isScalarCategory shouldBe (false)
  }

  test("test simple conversion") {
    USD in USD shouldEqual Right(BigDecimal(1.0))
    USD in US_CENT shouldEqual Right(BigDecimal(100))
    US_CENT in USD shouldEqual Right(BigDecimal(.01))

    (BBL / USD).in(GAL / USD) shouldEqual Right(BigDecimal(42.0))
    (USD / BBL).in(USD / GAL) shouldEqual Right(1 / BigDecimal(42.0))
    (USD / (BBL * BBL)).in(USD / (GAL * GAL)) shouldEqual Right(1 / BigDecimal(42.0 * 42.0))

    BBL * BBL in GAL shouldBe 'left

    USD / BBL in USD shouldBe 'left

    USD / (BBL * BBL) in USD / GAL shouldBe 'left
    SCALAR in PERCENT shouldEqual Right(BigDecimal(100.0))
    SCALAR in USD / GAL shouldBe 'left
  }



  test("test custom conversion") {
    val conv = DimensionConversions.fromMap(Map((MT, BBL) -> 7.45)).get
    BBL in MT should be ('left)
    BBL in (MT, conv) shouldEqual Right(1 / BigDecimal(7.45))
    MT in (BBL, conv) shouldEqual Right(BigDecimal(7.45))
  }

  test("numerator") {
    (USD/BBL).numerator shouldEqual USD
    (BBL/USD).numerator shouldEqual BBL
    (BBL*BBL/USD).numerator shouldEqual BBL*BBL
    (BBL*GAL/USD).numerator shouldEqual BBL*GAL
    SCALAR.numerator shouldEqual SCALAR
  }

  test("denominator") {
    (USD/BBL).denominator shouldEqual BBL
    (BBL/USD).denominator shouldEqual USD
    (BBL*BBL/USD).denominator shouldEqual USD
    (BBL/(USD*USD)).denominator shouldEqual USD*USD
    (BBL/(G*USD)).denominator shouldEqual G*USD
    (SCALAR/USD).denominator shouldEqual USD
  }

  test("pow") {
    USD.pow(0) shouldEqual SCALAR
    USD.pow(1) shouldEqual USD
    USD.pow(2) shouldEqual USD*USD
    USD.pow(5) shouldEqual USD*USD*USD*USD*USD
    USD.pow(-1) shouldEqual SCALAR/USD
    USD.pow(-2) shouldEqual SCALAR/(USD*USD)
  }

  test("ccy") {
    USD.isCcy shouldEqual true
    GBP.isCcy shouldEqual true
    (USD/GBP).isCcyPair shouldEqual true
  }

  test("ccy pair with cents toString") {
    (GBP/US_CENT).toString shouldEqual "¢GBP" // bit weird this one. but we should never see it unless we have a bug.
  }

  test("Arithmetic caching") {
    USD.invert.invert should be theSameInstanceAs USD
    (USD / MT) should be theSameInstanceAs (MT / USD).invert
    ((USD / MT) * MT) should be theSameInstanceAs USD
  }

  test("to/from string") {
    val table = Table(
      ("Text", "UOM"),
      ("WSC", WSC),
      ("USD", USD),
      ("USD^1", USD),
      ("USD^2", USD * USD),
      ("USD^-1", SCALAR/USD),
      ("USD/BBL", USD/BBL),
      ("USD^2/BBL^2", (USD*USD)/(BBL*BBL)),
      ("USD^3/BBL^3", (USD*USD*USD)/(BBL*BBL*BBL)),
      ("USD^-2/BBL^-2", (BBL*BBL)/(USD*USD)),
      ("¢/GAL", US_CENT/GAL),
      ("GAL^2/¢", GAL*GAL/US_CENT),
      ("USD BBL/GBP", USD * BBL / GBP),
      ("USD BBL^2/GBP", USD * BBL * BBL / GBP),
      ("KBBL", K_BBL),
      ("HMT", H_MT),
      ("USDGBP", GBP/USD),
      ("USD/GBP Day", USD / GBP / DAY),
      ("M3", M3),
      ("CM3", CM3),
      ("pp", IMM_PRICE_POINT)
    )
    forAll(table) {
      case (text, uom) => 
        if (! (text matches UOM.Regex.regex))
          fail(s"$text did not match UOM regex")

        UOM.fromString(text).R shouldEqual (uom)
        UOM.fromString(uom.toString).R shouldEqual (uom)
    }
  }
  test("uom from invalid string") {
    UOM.fromString("BLAH") should be ('left)
    UOM.fromString("BLAH/BLAH") should be ('left)
    UOM.fromString("BLAH^-2") should be ('left)
    UOM.fromString("123 USD") should be ('left)
  }

  test("power energy uom to and from string") {
    MWH.toString shouldEqual "MWH"
    (HOUR * MW).toString shouldEqual "MWH"

    UOM.fromStringOrThrow("MWH") shouldEqual MWH
    UOM.fromStringOrThrow("EUR/MWH") shouldEqual EUR/MWH
  }

  test("inMajorCcy") {
    UOM.NULL.inMajorCcy should be (UOM.NULL)
    SCALAR.inMajorCcy should be (SCALAR)
    MT.inMajorCcy should be (MT)
    US_CENT.inMajorCcy should be (USD)
    (US_CENT / GAL).inMajorCcy should be (USD / GAL)
    (US_CENT.pow(2) * EUR / GAL).inMajorCcy should be (USD * USD * EUR / GAL)
    WSC.inMajorCcy should be (WSC)
  }
  // This fails due to UOM's private constructor.... hmmm
  ignore("readResolve") {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(USD)
    oos.close()

    val byteArray = baos.toByteArray
    val obj = new ObjectInputStream(new ByteArrayInputStream(byteArray)).readObject
    obj should be theSameInstanceAs USD
  }

}
