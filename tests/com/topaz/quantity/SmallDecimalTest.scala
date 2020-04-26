package com.topaz.quantity

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.tags.Slow
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.math.BigDecimal.RoundingMode

@Slow
class SmallDecimalTest extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks {
  implicit val valueGen = for {
      value <- Gen.chooseNum(-1e10, 1e10)
      valueDecimal = BigDecimal.valueOf(value).setScale(6, RoundingMode.HALF_UP)
      if SmallDecimal.canRepresent(valueDecimal)
  } yield valueDecimal
  
  
  test("constructors") {
    SmallDecimal.canRepresent(BigDecimal(SmallDecimal.MAX_LONG)) shouldBe true
    SmallDecimal.canRepresent(BigDecimal(SmallDecimal.MAX_LONG + 1)) shouldBe false
    SmallDecimal.canRepresent(BigDecimal(SmallDecimal.MIN_LONG)) shouldBe true
    SmallDecimal.canRepresent(BigDecimal(SmallDecimal.MIN_LONG - 1)) shouldBe false

    forAll(Gen.chooseNum(SmallDecimal.MIN_LONG, SmallDecimal.MAX_LONG), minSuccessful(100 * 1000)) {
      (a) =>
        SmallDecimal.of(a.toInt) shouldEqual SmallDecimal.of(BigDecimal(a.toInt))
        SmallDecimal.of(a) shouldEqual SmallDecimal.of(BigDecimal(a))
    }
  }

  test("arithmetic") {
    def ignoreOverflow(f: => Unit) = try {
      // we test overflow conditions elsewhere
      f
    } catch {
      case _: ArithmeticException =>
    }
    // check that SmallDecimal's arithmetic matches BigDecimal's using randomly generated values
    forAll(valueGen, valueGen, minSuccessful(100 * 1000)) {
      (a, b) =>
        val aa = SmallDecimal.of(a)
        val bb = SmallDecimal.of(b)
        ignoreOverflow {
          (aa + bb).bigDecimalValue shouldEqual (a + b)
        }
        ignoreOverflow {
          (aa - bb).bigDecimalValue shouldEqual (a - b)
        }
        ignoreOverflow {
          (aa * bb).bigDecimalValue shouldEqual (a * b)
        }
        ignoreOverflow {
          (aa / bb).bigDecimalValue shouldEqual (a / b)
        }
    }
  }

  test("mult") {
    val data = List(
      ("-10000000000.0", "10000000000.0", true),
      ("9151397667784.7041", "1", false),
      ("9151397667784.7041", ".1", false),
      ("9151397667784.7041", "10", true),
      ("1", "9151397667784.7041", false),
      ("100", "9151397667784.7041", true),
      (SmallDecimal.MAX_SD.toString, "2", true),
      ((SmallDecimal.MAX_SD.bigDecimalValue / 2).setScale(6, RoundingMode.DOWN).toString(), "2", false)
    )
    data.foreach {
      case (x, y, shouldOverflow) if shouldOverflow =>
        intercept[ArithmeticException] {
          SmallDecimal.of(x) * SmallDecimal.of(y)
        }
      case (x, y, shouldOverflow) =>
        (SmallDecimal.of(x) * SmallDecimal.of(y)).bigDecimalValue shouldEqual BigDecimal(x) * BigDecimal(y)
    }
  }

  test("div") {
    val data = List(
      ("123", "30", false),
      ("123", "300", false),
      ("123", "3000", false),
      ("10000", "10", false),
      ("1", ".1", false),
      ("1", "5", false),
      ("9151397667784.7041", "2", false),
      ("1", "3", true),
      (SmallDecimal.MAX_SD.toString, ".1", true)
    )
    data.foreach {
      case (x, y, shouldOverflow) if shouldOverflow =>
        intercept[ArithmeticException] {
          SmallDecimal.of(x) / SmallDecimal.of(y)
        }
      case (x, y, shouldOverflow) =>
        (SmallDecimal.of(x) / SmallDecimal.of(y)).bigDecimalValue shouldEqual BigDecimal(x) / BigDecimal(y)
    }
  }

  test("add") {
    val data = List(
      // column 0 + column 1 and check if error thrown (true) or not (false)
      (SmallDecimal.MAX_SD.toString, "1", true),
      (SmallDecimal.MIN_SD.toString, "1", false),
      (SmallDecimal.MAX_SD.toString, "0", false),
      (SmallDecimal.MIN_SD.toString, "0", false),
      (SmallDecimal.MAX_SD.toString, "-1", false),
      (SmallDecimal.MIN_SD.toString, "-1", true)
    )
    data.foreach {
      case (x, y, shouldOverflow) if shouldOverflow =>
        withClue(s"$x + $y") {
          intercept[ArithmeticException] {
            SmallDecimal.of(x) + SmallDecimal.of(y)
          }
        }
      case (x, y, shouldOverflow) =>
        (SmallDecimal.of(x) + SmallDecimal.of(y)).bigDecimalValue shouldEqual BigDecimal(x) + BigDecimal(y)
    }
  }

  test("subtract") {
    // column 0 - column 1 and check if error thrown (true) or not (false)
    val data = List(
      (SmallDecimal.MAX_SD.toString, "1", false),
      (SmallDecimal.MIN_SD.toString, "1", true),
      (SmallDecimal.MAX_SD.toString, "0", false),
      (SmallDecimal.MIN_SD.toString, "0", false),
      (SmallDecimal.MAX_SD.toString, "-1", true),
      (SmallDecimal.MIN_SD.toString, "-1", false)
    )
    data.foreach {
      case (x, y, shouldOverflow) if shouldOverflow =>
        withClue(s"$x - $y") {
          intercept[ArithmeticException] {
            SmallDecimal.of(x) - SmallDecimal.of(y)
          }
        }
      case (x, y, shouldOverflow) =>
        (SmallDecimal.of(x) - SmallDecimal.of(y)).bigDecimalValue shouldEqual BigDecimal(x) - BigDecimal(y)
    }
  }
}
