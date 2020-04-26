package com.topaz.quantity

import java.text.DecimalFormat

import scalaz.syntax.std.boolean._

/**
 * Represents a decimal value with up to SmallDecimal.SCALE places
 * after the decimal point.
 *
 * With a SCALE of 6, the max value it can represent is 9223372036854.775807
 *
 * Any operations that would result in an overflow of `value` throws
 * an ArithmeticException. If you would lose precision it also throws.
 *
 */
case class SmallDecimal(value: Long) extends AnyVal {
  def signum = value.signum
  def isZero = value == 0

  def +(other: SmallDecimal): SmallDecimal = {
    SmallDecimal(SafeLongMath.safeAdd(value, other.value))
  }

  def -(other: SmallDecimal): SmallDecimal = {
    SmallDecimal(SafeLongMath.safeSubtract(value, other.value))
  }

  def *(other: SmallDecimal): SmallDecimal = {
    if(!SafeLongMath.multWouldFail(value, other.value)) {
      val mult = SmallDecimal(value * other.value)
      mult divLong SmallDecimal.SCALE
    } else {
      // this gives us a chance to multiply, for example, 9151397667784.704 by 1
      // and not overflow.
      val (newVal, newValScaled) = withTrailingZerosRemoved
      val (newOther, newOtherScaled) = other.withTrailingZerosRemoved
      val mult = SmallDecimal(SafeLongMath.safeMult(newVal, newOther))
      val newScale = SmallDecimal.DPS - newOtherScaled - newValScaled
      if(newScale >= 0)
        mult divLong safe10toPow(newScale)
      else
        copy(SafeLongMath.safeMult(mult.value, safe10toPow(newScale)))
    }
  }

  def /(other: SmallDecimal): SmallDecimal = {
    if(!SafeLongMath.divWouldLosePrecision(value, other.value)) {
      val div = SmallDecimal(value / other.value)
      div multLong SmallDecimal.SCALE
    } else {
      // if we didn't do this bit, we couldn't do, for example, 1 / 5
      val (newOther, zeros) = other.withTrailingZerosRemoved
      val newPow = SmallDecimal.DPS - zeros
      if (newPow >= 0)
        (this divLong newOther) multLong safe10toPow(newPow)
      else
        (this divLong newOther) divLong safe10toPow(newPow)
    }
  }

  def multLong(mult: Long): SmallDecimal = copy(SafeLongMath.safeMult(value, mult))

  def divLong(div: Long): SmallDecimal = copy(SafeLongMath.safeDiv(value, div))

  def abs: SmallDecimal = copy(SafeLongMath.safeAbs(value))

  def doubleValue: Double = value * 1.0 / SmallDecimal.SCALE
  def bigDecimalValue: BigDecimal = {
    BigDecimal.valueOf(value).bigDecimal.movePointLeft(SmallDecimal.DPS).stripTrailingZeros()
  }

  def negate: SmallDecimal = copy(SafeLongMath.safeNegate(value))

  override def toString: String = SmallDecimal.FORMAT.format(bigDecimalValue)

  private def safe10toPow(n: Int) = {
    val res = math.pow(10, n.abs)
    if(res > Long.MaxValue.toDouble) {
      throw SDArithmeticFail(s"Arithmetic operation would overflow (10^${n.abs})")
    }
    res.toLong
  }

  private def withTrailingZerosRemoved: (Long, Int) = {
    var newValue: Long = value
    var scaled = 0
    while(newValue != 0 && newValue % 10 == 0) {
      newValue /= 10
      scaled += 1
    }
    (newValue, scaled)
  }
}

private object SafeLongMath {
  def addWouldFail(x: Long, y: Long): Boolean = {
    val result = x + y
    ((x ^ result) & (y ^ result)) < 0
  }

  def subWouldFail(x: Long, y: Long): Boolean = {
    val result = x - y
    ((x ^ y) & (x ^ result)) < 0
  }

  def multWouldFail(x: Long, y: Long): Boolean = {
    val r = x * y
    val ax = Math.abs(x)
    val ay = Math.abs(y)
    if (((ax | ay) >>> 31) != 0) {
      ((y != 0) && (r / y != x)) || (x == Long.MinValue && y == -1)
    } else {
      false
    }
  }

  def divWouldLosePrecision(x: Long, y: Long) = x % y != 0

  def safeAdd(x: Long, y: Long): Long = 
    if(addWouldFail(x, y)) throw SDArithmeticFail("Not safe to add") else x + y

  def safeSubtract(x: Long, y: Long): Long = 
    if(subWouldFail(x, y)) throw SDArithmeticFail("Not safe to sub") else x - y

  def safeMult(x: Long, y: Long): Long = 
    if(multWouldFail(x, y)) throw SDArithmeticFail("Not safe to mult") else x * y

  def safeDiv(x: Long, y: Long): Long = if (SafeLongMath.divWouldLosePrecision(x, y))
    throw SDArithmeticFail(s"Divide would lose precision: $x/$y")
  else
    x / y

  def safeNegate(x: Long) = {
    if (x == Long.MinValue) {
      throw SDArithmeticFail("Operation would overflow")
    }
    -x
  }

  def safeAbs(x: Long) = if(x < 0) safeNegate(x) else x

  def noArithmeticException(f: => Unit) = !throwsArithmeticException(f)

  def throwsArithmeticException[A](f: => A): Boolean = {
    try {
      f
      false
    }
    catch {
      case _: ArithmeticException => true
    }
  }

}

/**
 * SmallDecimal is an AnyVal for speed. The +*-/ methods return SmallDecimals
 * so that we stay as longs during the calculations. Ideally they would return
 * Option or Either so that we could do fast checking of valid calculations.
 * Instead we use Exceptions, which is a bit evil. Exceptions are also slow if you
 * fill in the stack trace so we don't. We're using them for flow control so this
 * is ok (although evil as mentioned).
 */
case class SDArithmeticFail(s: String) extends ArithmeticException {
  override def fillInStackTrace(): Throwable = this
}

object SmallDecimal {
  import SafeLongMath._

  val MAX_BD = BigDecimal(Long.MaxValue)
  val MIN_BD = BigDecimal(Long.MinValue)

  val DPS: Int = 6
  val SCALE: Int = 1e6.toInt
  val MAX_SD = SmallDecimal.of(MAX_BD.bigDecimal.movePointLeft(DPS))
  val MIN_SD = SmallDecimal.of(MIN_BD.bigDecimal.movePointLeft(DPS))
  val MAX_LONG = MAX_BD.bigDecimal.movePointLeft(DPS).longValue()
  val MIN_LONG = MIN_BD.bigDecimal.movePointLeft(DPS).longValue()

  val FORMAT = new DecimalFormat("#0.######")

  def of(value: Int): SmallDecimal = {
    new SmallDecimal(value.toLong * SCALE)
  }

  def of(value: Long): SmallDecimal = {
    if(canRepresent(value))
      new SmallDecimal(value * SCALE)
    else
      throw new Exception(s"$value cannot be represented by SmallDecimal")
  }

  def of(value: String): SmallDecimal = {
    of(BigDecimal(value))
  }

  def of(value: BigDecimal): SmallDecimal = {
    maybe(value).getOrElse(sys.error(s"$value cannot be represented by SmallDecimal"))
  }

  def maybe(value: BigDecimal): Option[SmallDecimal] = {
    lazy val longValue = value.longValue()
    lazy val sd = SmallDecimal(value.bigDecimal.movePointRight(DPS).longValue)
    
    if ((value.signum == 0 || value.scale <= 0) && canRepresent(longValue)) {
      Some(of(longValue))
    } else if (value.compare(MAX_BD) == -1 &&
      value.compare(MIN_BD) == 1 &&
      sd.bigDecimalValue.compare(value) == 0) {
      Some(sd)
    } else {
      None
    }
  }

  def unapply(s: String): Option[SmallDecimal] = maybe(BigDecimal(s))

  def unapply(d: Double): Option[SmallDecimal] = maybe(d)

  def canRepresent(value: BigDecimal): Boolean = maybe(value).isDefined

  private def canRepresent(value: Long): Boolean = {
    value <= MAX_LONG && value >= MIN_LONG
  }
  
  def maybeSafeMult(a: SmallDecimal, b: SmallDecimal): Option[SmallDecimal] = {
    try {
      Some(a * b)
    } catch {
      case _: ArithmeticException => None
    }
  }

  def isSafeToMult(vals: SmallDecimal*): Boolean = {
    noArithmeticException(
      vals.foldLeft(SmallDecimal.of(1)) {
        case (acc, v) => acc * v
      }
    )
  }
  def isSafeToAdd(x: SmallDecimal, y: SmallDecimal) = noArithmeticException(x + y)
  def isSafeToDiv(x: SmallDecimal, y: SmallDecimal) = noArithmeticException(x / y)
  def isSafeToDiv(x: SmallDecimal, y: Int) = noArithmeticException(x divLong y)
}
