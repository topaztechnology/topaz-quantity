package com.topaz.quantity

import java.text.DecimalFormat

import com.topaz.TopazCodingError
import com.topaz.maths.Numberlike
import com.topaz.quantity.DimensionConversions._
import com.topaz.quantity.UOM._
import com.topaz.utils._

import scala.language.implicitConversions
import scala.math.BigDecimal.RoundingMode
import scala.math.{BigDecimal, Ordered}

case class NamingEnabled(b: Boolean) extends AnyVal

sealed trait Qty extends Ordered[Qty] with EitherPimpsMixin {
  def +(other: Qty): Qty
  def -(other: Qty): Qty
  def *(other: Qty): Qty
  def /(other: Qty): Qty

  def signum: Int
  def isZero: Boolean
  def isNonZero: Boolean = !isZero
  def isOne: Boolean
  def isAlmostZero: Boolean
  def doubleValue: Double
  def isPositive: Boolean = doubleValue > 0
  def isNegative: Boolean = doubleValue < 0
  def isNonNegative: Boolean = doubleValue >= 0
  def checkedDouble(uom: UOM): Double = {
    require(this.uom == uom, "checkedDouble. UOMs don't match: " + (this.uom, uom))
    doubleValue
  }
  
  def checkedPercent: Double = {
    uom match {
      case PERCENT => doubleValue / 100.0
      case IMM_PRICE_POINT => doubleValue / 100.0
      case BASIS_POINT => doubleValue / 10000.0
      case SCALAR => doubleValue
      case WSC => doubleValue
      case _ => throw TopazCodingError(s"Expected scalar uom, got [$uom]")
    }
  }
  
  def bdCheckedPercent: BigDecimal = {
    uom match {
      case PERCENT => bdValue / 100
      case IMM_PRICE_POINT => bdValue / 100
      case BASIS_POINT => bdValue / 10000
      case SCALAR => bdValue
      case WSC => bdValue
      case _ => throw TopazCodingError(s"Expected scalar uom, got [$uom]")

    }
  }
  
  def bdValue: BigDecimal
  def checkedBDValue(uom: UOM): BigDecimal = {
    require(this.uom == uom, "checkedBDValue. UOMs don't match: " + (this.uom, uom))
    bdValue
  }

  def zero: FixedQty = Qty(0, uom, taint)
  def one: FixedQty = Qty(1, uom, taint)
  def dblQty: DblQty
  def maybeDblQty: Qty = {
    if (isNull)
      this
    else
      dblQty
  }
  def uom: UOM
  def negate: Qty
  def unary_- : Qty
  def invert: Qty
  def inverse: Qty = invert
  def abs: Qty
  def max(rhs: Qty): Qty
  def min(rhs: Qty): Qty

  def ccy1: UOM = uom.ccy1
  def ccy2: UOM = uom.ccy2
  def priceMajorCCY: UOM = uom.priceMajorCCY

  def ^+ : Qty = max(Qty(0.0, uom, taint))

  def in(other: UOM, conv: DimensionConversions = empty): Either[TopazFail, Qty]
  def convertWithFXRate(fxRate: Qty): Either[TopazFail, Qty] = {
    val numerator = uom.numerator.inMajorCcy
    if (numerator == fxRate.ccy1)
      Right(this * fxRate)
    else if (numerator == fxRate.ccy2)
      Right(this * fxRate.inverse)
    else
      TopazFail(s"Cannot convert $this using $fxRate")
  }
  override def toString: String = toFormattedString(4, useBrackets = false)

  def toS(dp: Int): String = toFormattedString(dp, useBrackets = false)
  
  def toMinimalString: String = toFormattedString("#,###.################", useBrackets = false)
  def toStringAllDecimalPlaces: String = bdValue.bigDecimal.toPlainString + (if (uom == SCALAR) "" else " " + uom)

  def toFormattedString(dp: Int, useBrackets: Boolean): String = if (dp < 16) { // DecimalFormat doesn't work after 16 decimal places
    toFormattedString("#,###" + (if (dp > 0) "." + "0" * dp else ""), useBrackets)
  } else {
    bdValue.bigDecimal.toPlainString + (if (uom == SCALAR) "" else " " + uom)
  }
  
  def toFormattedString(decimalFormat: String, useBrackets: Boolean, showUOMs: Boolean = true): String = {
    val format = new DecimalFormat(decimalFormat)
    format.setRoundingMode(java.math.RoundingMode.HALF_UP)
    if(useBrackets) {
      format.setNegativePrefix("(")
      format.setNegativeSuffix(")")
    } else {
      format.setNegativePrefix("-")
      format.setNegativeSuffix("")
    }
    format.format(bdValue) + (if (uom == SCALAR || !showUOMs) "" else " " + uom)
  }

  def isFixedPoint: Boolean
  def ensuringFixedPoint: Qty
  def isNull = uom == UOM.NULL
  def isInLots = uom == UOM.LOTS
  def isScalarCategory: Boolean = uom.isScalarCategory
  def isWorldscale: Boolean = uom == UOM.WSC
  def money: Qty = {
    uom.requireIsCurrency()
    this
  }
  def asPercent: Qty
  def asWorldscale: Qty

  def taint: Taint
  def hasTaint(value: Taint): Boolean = this.taint.hasTaint(value)
  def withTaint(newTaint: Taint): Qty
  def withTaint(maybeTaint: Option[Taint]): Qty = maybeTaint.map(withTaint).getOrElse(this)

  /**
    * Adds the 'interesting' taint to this quantity.
    * Used by naming to indicate that this level of the explanation is interesting to a user and any level
    * without `interesting` set will not be expanded by default.
    */
  def interesting: Qty = withTaint(Taint.interestingExplanation)

  def toSDQty: Either[TopazFail, SDQty] = round(SmallDecimal.DPS) match {
    case sd: SDQty => Right(sd)
    case o => TopazFail(s"Couldn't convert $this to and SDQty, got $o instead.")
  }

  /**
    * Same as round(maxDP) but easier to find when you want a fixed qty.
    *
    * Behaviour is:
    *  maxDP <= SmallDecimal.DPS && value < SmallDecimal.MAX_SD && value > SmallDecimal.MIN_SD - you get an SDQty
    *  otherwise you get a BDQty
    */
  def toFixedQty(maxDp: Int = SmallDecimal.DPS): FixedQty = round(maxDp)

  def round(nDP: Int): FixedQty

  def significantFigures(nSF: Int): FixedQty

  def inBaseCcy: Qty = {
    if (isNull)
      this
    else
      in(uom.inMajorCcy) match {
        case Right(q) => q
        case Left(e) => throw TopazCodingError(s"Shouldn't be possible to reach - converting [$this] to base ccy")
      }
  }

  def compare(that: Qty) = {
    require(that.uom == uom, s"compare. UOMs don't match: $this, $that")
    doubleValue.compare(that.doubleValue)
  }

  /*
   * Convenience methods that allow us to test for positivity etc, with natural
   * looking code, but without implicit conversions
   */
  private def compareWithZero(zero: Int, comparison: (Double, Int) => Boolean) = {
    require(zero == 0, s"Convenience method for comparing $this with 0, no other number allowed")
    comparison(doubleValue, zero)
  }
  def > (zero: Int) = compareWithZero(zero, _ > _)
  def >= (zero: Int) = compareWithZero(zero, _ >= _)
  def < (zero: Int) = compareWithZero(zero, _ < _)
  def <= (zero: Int) = compareWithZero(zero, _ <= _)


  def almostEquals(other: Qty, tolerance: Double) = {
    val thisDoubleValue = this.doubleValue
    val otherDoubleValue = other.doubleValue
    this.uom == other.uom &&
      DoubleUtils.almostEquals(thisDoubleValue, otherDoubleValue, tolerance)
  }

  /**
    * Copies this Qty value and returns the same Qty type but with the new UOM.
    * Note: The NULL Qty is not affected by this and will still return NULL
    */
  def withUOM(newUOM: UOM): Qty

  def named(name: => String)(implicit namingEnabled: NamingEnabled): Qty = {
    if (namingEnabled.b)
      NamedQty(name, this)
    else
      this
  }

  /**
    * Discard any old naming information in this quantity and start again with this new name
    */
  def newNamed(name: => String)(implicit namingEnabled: NamingEnabled): Qty = {
    if (namingEnabled.b)
      NamedQty(name, this.notNamed())
    else
      this
  }

  def functionNamed(name: => String, renderInline: Boolean, components: => Seq[Qty])
    (implicit namingEnabled: NamingEnabled): Qty = {

    if (namingEnabled.b) {
      FunctionNamedQty(name, renderInline, components.map(_.asNamed()), this.notNamed())
    } else
      this
  }

  def asNamed(name: => String): NamedQty = this match {
    case n: NamedQty => n
    case _ => NamedQty(name, this)
  }

  protected[quantity] def asNamed(): NamedQty = asNamed(NamedQty.format(this))

  def notNamedIfScalarOne(): Qty = {
    if (toFixedQty().isOne && isScalarCategory)
      notNamed()
    else
      this
  }

  def notNamed(): Qty = this match {
    case f: NamedQty => f.qty
    case _ => this
  }

  /**
    * This checks if the name of the named quantity is more than just
    * the string representation of the quantity.
    * Used to make sure explanations don't contain too much useless info.
    */
  def isProperlyNamed: Boolean = this match {
    case n: NamedQty => n.name != NamedQty.format(n.qty)
    case _ => false
  }
}

class DblQty (private val value: Double, val uom: UOM, val taint: Taint) extends Qty {
  require(!value.isNaN && !value.isInfinity, "Invalid value: " + value)
  require(uom != NULL, "Invalid, can't create a DblQty with NULL uom.")

  def signum: Int = value.signum
  def isZero: Boolean = value == 0.0
  def isOne: Boolean = value == 1.0
  def isAlmostZero: Boolean = DoubleUtils.almostEquals(value, 0.0, 1e-9)
  def +(other: Qty) = {
    if (other.isNull) {
      this
    } else {
      uom.addOrSubtract(other.uom) match {
        case Right(None) => new DblQty(value + other.doubleValue, uom, taint | other.taint)
        case Right(Some(scale)) => new DblQty(value + (other.doubleValue / scale.doubleValue()), uom, taint | other.taint)
        case Left(error) => sys.error(s"${error.s} when adding [$this] and [$other]")
      }
    }
  }

  def -(other: Qty): Qty = this.+(other.negate)

  def *(other: Qty): Qty = uom.mult(other.uom) match {
    case (newUOM, mult) => new DblQty(value * other.doubleValue * mult.doubleValue(), newUOM, taint | other.taint)
  }

  def /(other: Qty): Qty = {
    if (other.isZero) throw new ArithmeticException("Division by zero")
    this.*(other.invert)
  }

  def negate = new DblQty(value * -1, uom, taint)
  override def unary_- = negate

  def invert = new DblQty(1 / value, uom.invert, taint)
  def abs = new DblQty(math.abs(value), uom, taint)
  def max(rhs: Qty): Qty = if (doubleValue >= rhs.checkedDouble(uom)) this else rhs
  def min(rhs: Qty): Qty = if (doubleValue <= rhs.checkedDouble(uom)) this else rhs

  override def in(other: UOM, conv: DimensionConversions): Either[TopazFail, DblQty] = {
    uom.in(other, conv).map(scale => Qty(this.value * scale.toDouble, other, taint))
  }

  def doubleValue = value
  override def bdValue = BigDecimal(value)

  override def dblQty: DblQty = this

  override def hashCode() = value.hashCode() ^ uom.hashCode()

  override def equals(obj: Any) = obj match {
    case other: Qty if this.isScalarCategory && other.isScalarCategory => checkedPercent == other.checkedPercent
    case other: Qty => value == other.doubleValue && uom == other.uom
    case _ => false
  }

  override def isFixedPoint = false
  def ensuringFixedPoint: Qty = throw new RuntimeException("Not fixed point Qty: " + this)

  def round(nDP: Int) = Qty(value.toString, uom, taint).round(nDP)

  def significantFigures(nSF: Int): FixedQty = Qty(value.toString, uom).significantFigures(nSF)

  def asPercent: DblQty = {
    require(isScalarCategory, s"$this is not a scalar so cannot be shown as a percentage")
    in(PERCENT).get
  }

  def asWorldscale: DblQty = {
    require(isScalarCategory, s"$this is not a scalar so cannot be shown as WSC")
    in(WSC).get
  }

  def withUOM(newUOM: UOM): Qty = if (isNull) this else new DblQty(value, newUOM, taint)

  def withTaint(newTaint: Taint): DblQty = new DblQty(value, uom, taint | newTaint)
}

sealed trait FixedQty extends Qty {
  override def isFixedPoint = true
  def ensuringFixedPoint: FixedQty = this

  def mult(other: FixedQty): FixedQty = this.*(other).asInstanceOf[FixedQty]
  def *(n: Int): FixedQty = if (uom == UOM.PERCENT) mult(Qty(n, SCALAR, taint)).asPercent else mult(Qty(n, SCALAR, taint))
  def plus(other: FixedQty): FixedQty = this.+(other).asInstanceOf[FixedQty]
  def minus(other: FixedQty): FixedQty = this.-(other).asInstanceOf[FixedQty]
  def div(other: FixedQty): FixedQty = this./(other).asInstanceOf[FixedQty]
  def div(other: Int): FixedQty = this div Qty(other, SCALAR, taint)

  def in(other: UOM, conv: DimensionConversions = empty): Either[TopazFail, FixedQty]
  override def invert: FixedQty
  def negate: FixedQty
  override def unary_- : FixedQty = negate
  def bdQty: BDQty
  override def inBaseCcy: FixedQty = {
    if (isNull)
      this
    else
      in(uom.inMajorCcy) match {
        case Right(q) => q
        case Left(e) =>
          throw TopazCodingError(s"Shouldn't be possible to reach this - converting [$this] to base ccy")
      }
  }

  def asPercent: FixedQty = {
    require(isScalarCategory, s"$this is not a scalar so cannot be shown as a percentage")
    in(PERCENT).get
  }

  def asWorldscale: FixedQty = {
    require(isScalarCategory, s"$this is not a scalar so cannot be shown as WSC")
    in(WSC).get
  }

  def withUOM(newUOM: UOM): FixedQty

  def abs: FixedQty

  def withTaint(newTaint: Taint): FixedQty
}

object FixedQty {
  def apply(value: BigDecimal, uom: UOM): FixedQty = {
    Qty(value, uom, Taint.clean)
  }

  def apply(value: BigDecimal, uom: UOM, taint: Taint): FixedQty = {
    Qty(value, uom, taint)
  }

  def sum(qtys: Iterable[FixedQty]): FixedQty = {
    qtys.foldLeft[FixedQty](Qty.NULL)(_ plus _)
  }

}

object BDQty {
  def apply(value: String, uom: UOM): BDQty = apply(value, uom, Taint.clean)

  def apply(value: String, uom: UOM, taint: Taint): BDQty = {
    new BDQty(BigDecimal(value), uom, taint)
  }
}

class BDQty private[quantity] (val value: BigDecimal, val uom: UOM, val taint: Taint) extends FixedQty {
  require(uom != NULL, "Invalid, can't create a BDQty with NULL uom.")

  def signum: Int = value.signum
  def isZero: Boolean = value.doubleValue == 0.0
  def isOne: Boolean = value.doubleValue == 1.0
  def isAlmostZero: Boolean = DoubleUtils.almostEquals(value.doubleValue, 0.0, 1e-9)

  def +(other: Qty): Qty = {
    if (other.isNull) {
      this
    } else {
      (this, other) match {
        case (_, _: DblQty) => dblQty.+(other)
        case _ => uom.addOrSubtract(other.uom) match {
          case Right(None) => FixedQty(value + other.bdValue, uom, taint | other.taint)
          case Right(Some(scale)) => FixedQty(value + (other.bdValue / scale), uom, taint | other.taint)
          case Left(error) => sys.error(s"${error.s} when adding [$this] and [$other]")
        }
      }
    }
  }

  def -(other: Qty): Qty = this.+(other.negate)

  def *(other: Qty): Qty = other match {
    case _: DblQty => dblQty.*(other)
    case _ => uom.mult(other.uom) match {
      case (newUOM, com.topaz.quantity.QtyUtils.BDOne) => FixedQty(value * other.bdValue, newUOM, taint | other.taint)
      case (newUOM, mult) => FixedQty(value * other.bdValue * mult, newUOM, taint | other.taint)
    }
  }

  def /(other: Qty): Qty = {
    if (other.isZero) throw new ArithmeticException("Division by zero")
    other match {
      case _: DblQty => dblQty./(other)
      case _ => uom.div(other.uom) match {
        case (newUOM, com.topaz.quantity.QtyUtils.BDOne) => FixedQty(value / other.bdValue, newUOM, taint | other.taint)
        case (newUOM, mult) => FixedQty(value / other.bdValue * mult, newUOM, taint | other.taint)
      }
    }
  }

  def negate: FixedQty = FixedQty(value * BigDecimal(-1), uom, taint)
  def invert: FixedQty = FixedQty(com.topaz.quantity.QtyUtils.BDOne / value, uom.invert, taint)
  def abs: FixedQty = FixedQty(value.abs, uom, taint)
  def max(rhs: Qty): Qty = if (bdValue >= rhs.checkedBDValue(uom)) this else rhs
  def min(rhs: Qty): Qty = if (bdValue <= rhs.checkedBDValue(uom)) this else rhs

  override def in(other: UOM, conv: DimensionConversions): Either[TopazFail, FixedQty] = {
    uom.in(other, conv).map(scale => FixedQty(this.value * scale, other, taint))
  }

  def doubleValue: Double = value.toDouble
  override def bdValue: scala.BigDecimal = value

  override def hashCode() = value.hashCode() ^ uom.hashCode()

  override def equals(obj: Any) = obj match {
    case other: Qty if this.isScalarCategory && other.isScalarCategory =>
      bdCheckedPercent.equals(other.bdCheckedPercent)
    case other: Qty => value.equals(other.bdValue) && uom == other.uom
    case _ => false
  }

  def dblQty: DblQty = new DblQty(value.toDouble, uom, taint)

  def round(nDP: Int): FixedQty = FixedQty(value.setScale(nDP, RoundingMode.HALF_UP), uom, taint)

  def significantFigures(nSF: Int): FixedQty = round(nSF - bdValue.precision + bdValue.scale)

  def bdQty: BDQty = this

  def withUOM(newUOM: UOM): BDQty = if (isNull) this else new BDQty(value, newUOM, taint)

  def withTaint(newTaint: Taint): BDQty = new BDQty(value, uom, taint | newTaint)
}


class SDQty(val value: SmallDecimal, val uom: UOM, val taint: Taint) extends FixedQty {
  if(uom == NULL) {
    require(value.isZero, s"Can't have non zero value with NULL uom: $value")
  }

  def signum: Int = value.signum
  def isZero: Boolean = value.isZero
  def isOne: Boolean = value == SmallDecimal.of(1)
  def isAlmostZero: Boolean = isZero

  def +(other: Qty): Qty = {
    if(this.isNull) {
      other
    } else if (other.isNull) {
      this
    } else {
      (this, other) match {
        case (_, _: DblQty) => dblQty.+(other)
        case _ =>
          (uom.addOrSubtract(other.uom), other) match {
            case (Right(None), otherSD: SDQty) =>
              if (SmallDecimal.isSafeToAdd(this.value, otherSD.value))
                new SDQty(value + otherSD.value, uom, taint | other.taint)
              else
                this + otherSD.bdQty

            case (Right(Some(scale)), otherSD: SDQty) =>
              if (scale.isValidInt && SmallDecimal.isSafeToDiv(otherSD.value, scale.intValue())) {
                this + new SDQty(otherSD.value divLong scale.intValue(), uom, taint | other.taint)
              } else {
                this + otherSD.bdQty
              }

            case (Right(None), _) =>
              new BDQty(value.bigDecimalValue + other.bdValue, uom, taint | other.taint)

            case (Right(Some(scale)), _) =>
              new BDQty(value.bigDecimalValue + (other.bdValue / scale), uom, taint | other.taint)

            case (Left(error), _) => sys.error(s"${error.s} when adding [$this] and [$other]")
          }
      }
    }
  }

  def -(other: Qty): Qty = this.+(other.negate)

  def *(other: Qty): Qty = {
    other match {
      case _: DblQty => dblQty.*(other)
      case otherSD: SDQty => uom.mult(other.uom) match {
        case (newUOM, QtyUtils.BDOne) => SmallDecimal.maybeSafeMult(value, otherSD.value) match  {
          case Some(newValue) => new SDQty(newValue, newUOM, taint | other.taint)
          case _ => this * otherSD.bdQty
        }
        case (newUOM, mult) =>
          SmallDecimal.maybe(mult).map {
            sdMult => if (SmallDecimal.isSafeToMult(value, otherSD.value, sdMult)) {
              new SDQty(value * otherSD.value * sdMult, newUOM, taint | other.taint)
            } else {
              this * otherSD.bdQty
            }
          }.getOrElse(this * otherSD.bdQty)
      }
      case _ => uom.mult(other.uom) match {
        case (newUOM, com.topaz.quantity.QtyUtils.BDOne) =>
          FixedQty(value.bigDecimalValue * other.bdValue, newUOM, taint | other.taint)
        case (newUOM, mult) =>
          FixedQty(value.bigDecimalValue * other.bdValue * mult, newUOM, taint | other.taint)
      }
    }
  }

  def /(other: Qty): Qty = {
    if (other.isZero) throw new ArithmeticException("Division by zero")
    other match {
      case _: DblQty => dblQty./(other)
      case otherSD: SDQty => uom.div(other.uom) match {
        case (newUOM, mult) =>
          SmallDecimal.maybe(mult).map {
            sdMult =>
              if (SmallDecimal.isSafeToDiv(value, otherSD.value) &&
                SmallDecimal.isSafeToMult(value / otherSD.value, sdMult)) {
              new SDQty((value / otherSD.value) * sdMult, newUOM, taint | other.taint)
            } else {
              this / otherSD.bdQty
            }
          }.getOrElse(this / otherSD.bdQty)
      }
      case _ => uom.div(other.uom) match {
        case (newUOM, com.topaz.quantity.QtyUtils.BDOne) =>
          FixedQty(value.bigDecimalValue / other.bdValue, newUOM, taint | other.taint)
        case (newUOM, mult) =>
          FixedQty(value.bigDecimalValue / other.bdValue * mult, newUOM, taint | other.taint)
      }
    }
  }

  def negate = new SDQty(value.negate, uom, taint)

  def invert: FixedQty = {
    if(SmallDecimal.isSafeToDiv(QtyUtils.SDOne, value))
      new SDQty(QtyUtils.SDOne / value, uom.invert, taint)
    else
      this.bdQty.invert
  }

  def abs = new SDQty(value.abs, uom, taint)
  def max(rhs: Qty): Qty = if (bdValue >= rhs.checkedBDValue(uom)) this else rhs
  def min(rhs: Qty): Qty = if (bdValue <= rhs.checkedBDValue(uom)) this else rhs

  override def in(other: UOM, conv: DimensionConversions): Either[TopazFail, FixedQty] = {
    if(other == this.uom) {
      Right(this)
    } else {
      uom.in(other, conv).map {
        scale =>
          val converted = SmallDecimal.maybe(scale).map {
            sdScale =>
              if (SmallDecimal.isSafeToMult(this.value, sdScale))
                new SDQty(this.value * sdScale, other, taint)
              else
                FixedQty(value.bigDecimalValue * sdScale.bigDecimalValue, other, taint)
          }
          converted.getOrElse(FixedQty(this.value.bigDecimalValue * scale, other, taint))
      }
    }
  }

  def doubleValue: Double = value.doubleValue
  override def bdValue: scala.BigDecimal = value.bigDecimalValue

  override def hashCode() = value.hashCode() ^ uom.hashCode()

  override def equals(obj: Any) = obj match {
    case other: SDQty if uom == other.uom =>
      value == other.value
    case other: Qty if this.isScalarCategory && other.isScalarCategory =>      /* Ensures 100% == 1 */
      bdCheckedPercent.equals(other.bdCheckedPercent)
    case other: SDQty => false                                                 /* Avoids creation of BigDecimals */
    case other: Qty =>
      uom == other.uom && value.bigDecimalValue.equals(other.bdValue)
    case _ => false
  }

  def dblQty: DblQty = new DblQty(doubleValue, uom, taint)

  def round(nDP: Int): FixedQty = if (nDP >= SmallDecimal.DPS || isNull) this else bdQty.round(nDP)

  def significantFigures(nSF: Int): FixedQty = bdQty.significantFigures(nSF)

  def bdQty: BDQty = new BDQty(value.bigDecimalValue, uom, taint)

  def sdValue = value

  def withUOM(newUOM: UOM): SDQty = if (isNull) this else new SDQty(value, newUOM, taint)

  def withTaint(newTaint: Taint): SDQty = new SDQty(value, uom, taint | newTaint)
}

object SDQty {
  def unapply(q: FixedQty): Option[SDQty] = q match {
    case s: SDQty => Some(s)
    case _ => None
  }
}

case class NamedQtyTreeNode(name: String, children: Seq[NamedQtyTreeNode], value: FixedQty) {
  override def toString: String = {
    // Sometimes we want to have a named quantity but no value. For example we have "47 times"
    // when building the time to expiry list for valuing an APO. To do that we do:
    // Qty.NULL.functionNamed("47 times", seqOfTimes)
    if (value.isNull) {
      name
    } else {
      val valueFormatted = NamedQty.format(value)
      
      if (name == valueFormatted) // We don't want to show "1.001 = 1.001"
        name
      else
        s"$name = $valueFormatted"
    }
  }

  def multilineString(lookAtShouldExpandFlag: Boolean): String = {
    val str = new StringBuilder(toString() + StringUtils.localNewline)

    def buildTree(node: NamedQtyTreeNode): Unit = {
      if (!lookAtShouldExpandFlag || node.shouldExpand) {
        node.children.foreach(n => str ++= n.toString() + StringUtils.localNewline)
        node.children.foreach(buildTree)
      }
    }

    buildTree(this)

    str.mkString
  }

  def shouldExpand: Boolean = {
    value.hasTaint(Taint.interestingExplanation)
  }
}

sealed trait NamedQty extends Qty with RichAnys {
  require(!qty.isInstanceOf[NamedQty], "NamedQty.qty can't be a named qty")

  def name: String

  def tree: NamedQtyTreeNode

  def qty: Qty

  def taint: Taint = qty.taint

  def signum: Int = qty.signum

  def isZero: Boolean = qty.isZero

  def isOne: Boolean = qty.isOne

  def isAlmostZero: Boolean = qty.isAlmostZero

  def doubleValue: Double = qty.doubleValue

  def bdValue: BigDecimal = qty.bdValue

  def dblQty: DblQty = qty.dblQty

  def uom: UOM = qty.uom

  def round(nDP: Int): FixedQty = RoundedNamedQty(nDP, this, qty.round(nDP))

  def significantFigures(nSF: Int): FixedQty = SignificantFiguresQty(nSF, this, qty.significantFigures(nSF))

  def max(rhs: Qty): Qty = FunctionNamedQty("Max", renderInline = true, Seq(this, rhs.asNamed()), qty.max(rhs.notNamed()))

  def min(rhs: Qty): Qty = FunctionNamedQty("Min", renderInline = true, Seq(this, rhs.asNamed()), qty.min(rhs.notNamed()))
}

object NamedQty {
  def apply(name: String, qty: Qty): NamedQty = qty match {
    case n: NamedQty if n.name == name => n
    case n: NamedQty => SingleQtyNamedQty(name, n, n.notNamed())
    case o => LeafQtyNamedQty(name, o)
  }

  val format: String = "#,##0.########"

  def format(qty: Qty): String = qty.toFormattedString(format, useBrackets = false)
}

sealed trait NamedQtyNotFixed extends NamedQty {
  def +(other: Qty): Qty = {
    val otherNotNamed = other.notNamed()
    this match {
      case BinaryOpNamedQty("+", lhs, rhs, otherQty) =>
        FunctionNamedQty("Sum", renderInline = false, Seq(lhs, rhs, other.asNamed()), otherQty + otherNotNamed)
      case FunctionNamedQty("Sum", renderInline, qtys, otherQty) =>
        FunctionNamedQty("Sum", renderInline, qtys :+ other.asNamed(), otherQty + otherNotNamed)
      case _ =>
        BinaryOpNamedQty("+", this, other.asNamed(), this.qty + otherNotNamed)
    }
  }

  def -(other: Qty): Qty = BinaryOpNamedQty("-", this, other.asNamed(), this.qty - other.notNamed())

  def *(other: Qty): Qty = {
    if (other.isOne && other.uom == SCALAR && !other.isProperlyNamed) {
      this
    } else {
      BinaryOpNamedQty("*", this, other.asNamed(), this.qty * other.notNamed())
    }
  }

  def /(other: Qty): Qty = {
    if (other.isOne && other.uom == SCALAR && !other.isProperlyNamed) {
      this
    } else {
      val otherNamedQty = other.notNamed()
      if (other.uom == SCALAR) {
        this match {
          case BinaryOpNamedQty("+", lhs, rhs, otherQty) if other.doubleValue.toInt == 2 =>
            FunctionNamedQty("Average", renderInline = false, Seq(lhs, rhs), otherQty / otherNamedQty)
          case FunctionNamedQty("Sum", renderInline, qtys, otherQty) if other.doubleValue.toInt == qtys.size =>
            FunctionNamedQty("Average", renderInline, qtys, otherQty / otherNamedQty)
          case _ =>
            BinaryOpNamedQty("/", this, other.asNamed(), this.qty / otherNamedQty)
        }
      } else {
        BinaryOpNamedQty("/", this, other.asNamed(), this.qty / otherNamedQty)
      }
    }
  }

  def negate: Qty = NegateNamedQty(this, qty.negate)

  def unary_- : Qty = negate

  def invert: Qty = InvertNamedQty(this, qty.invert)

  def abs: Qty = FunctionNamedQty("Abs", renderInline = true, Seq(this), qty.abs)

  def in(other: UOM, conv: DimensionConversions): Either[TopazFail, Qty] = {
    if (qty.uom == other)
      Right(this)
    else
      qty.in(other, conv).map(converted => FunctionNamedQty("ConvertTo", renderInline = true,
        Seq(Qty.NULL.asNamed(other.toString), this), converted))
  }

  def isFixedPoint: Boolean = qty.isFixedPoint

  def ensuringFixedPoint: Qty = qty.ensuringFixedPoint

  def asPercent: Qty = {
    require(isScalarCategory, s"$this is not a scalar so cannot be shown as a percentage")
    in(PERCENT).get
  }

  def asWorldscale: Qty = {
    require(isScalarCategory, s"$this is not a scalar so cannot be shown as WSC")
    in(WSC).get
  }

  def withUOM(newUOM: UOM): Qty = FunctionNamedQty(s"ToUOM", renderInline = true,
    Seq(Qty.NULL.asNamed(newUOM.toString), this), qty.withUOM(newUOM))
}

sealed trait NamedQtyFixed extends NamedQty with FixedQty {
  def +(other: Qty): Qty = {
    val otherNotNamed = other.notNamed().toFixedQty()
    this match {
      case BinaryOpFixedNamedQty("+", lhs, rhs, otherQty) =>
        FunctionFixedNamedQty("Sum", renderInline = false, Seq(lhs, rhs, other.asNamed()), otherQty plus otherNotNamed)
      case FunctionFixedNamedQty("Sum", renderInline, qtys, otherQty) =>
        FunctionFixedNamedQty("Sum", renderInline, qtys :+ other.asNamed(), otherQty plus otherNotNamed)
      case _ =>
        BinaryOpFixedNamedQty("+", this, other.asNamed(), this.qty plus otherNotNamed)
    }
  }

  def -(other: Qty): Qty = BinaryOpFixedNamedQty("-", this, other.asNamed(), this.qty minus other.notNamed().toFixedQty())

  def *(other: Qty): Qty = {
    if (this.isOne && uom == SCALAR) {
      other
    } else if (other.isOne && other.uom == SCALAR) {
      this
    } else {
      BinaryOpFixedNamedQty("*", this, other.asNamed(), this.qty mult other.notNamed().toFixedQty())
    }
  }

  def /(other: Qty): Qty = {
    if (other.isOne && other.uom == SCALAR) {
      this
    } else {
      val otherNotNamed = other.notNamed().toFixedQty()
      if (other.uom == SCALAR) {
        this match {
          case BinaryOpFixedNamedQty("+", lhs, rhs, otherQty) if other.doubleValue.toInt == 2 =>
            FunctionFixedNamedQty("Average", renderInline = true, Seq(lhs, rhs), otherQty div otherNotNamed)
          case FunctionFixedNamedQty("Sum", renderInline, qtys, otherQty) if other.doubleValue.toInt == qtys.size =>
            FunctionFixedNamedQty("Average", renderInline, qtys, otherQty div otherNotNamed)
          case _ =>
            BinaryOpFixedNamedQty("/", this, other.asNamed(), this.qty div otherNotNamed)
        }
      } else {
        BinaryOpFixedNamedQty("/", this, other.asNamed(), this.qty div otherNotNamed)
      }
    }
  }

  def bdQty: BDQty = qty.bdQty

  def qty: FixedQty

  def negate: FixedQty = NegateFixedNamedQty(this, qty.negate)

  def invert: FixedQty = InvertFixedNamedQty(this, qty.invert)

  def abs: FixedQty = FunctionFixedNamedQty("Abs", renderInline = true, Seq(this), qty.abs)

  def in(other: UOM, conv: DimensionConversions): Either[TopazFail, FixedQty] = {
    if (qty.uom == other)
      Right(this)
    else
      qty.in(other, conv).map(converted => FunctionFixedNamedQty(s"ConvertTo", renderInline = true,
        Seq(Qty.NULL.asNamed(other.toString), this), converted))
  }

  def withUOM(newUOM: UOM): FixedQty = FunctionFixedNamedQty(s"ToUOM", renderInline = true,
    Seq(Qty.NULL.asNamed(newUOM.toString), this), qty.withUOM(newUOM))
}

case class LeafQtyNamedQty(name: String, qty: Qty) extends NamedQtyNotFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Nil, qty.toFixedQty())

  def withTaint(taint: Taint): Qty = copy(qty = qty.withTaint(taint))
}

case class SingleQtyNamedQty(name: String, namedQty: NamedQty, qty: Qty) extends NamedQtyNotFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty.toFixedQty())

  def withTaint(taint: Taint): Qty = copy(qty = qty.withTaint(taint))
}

case class BinaryOpNamedQty(op: String, lhs: NamedQty, rhs: NamedQty, qty: Qty) extends NamedQtyNotFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(lhs.tree, rhs.tree), qty.toFixedQty())

  def withTaint(taint: Taint): Qty = copy(qty = qty.withTaint(taint))

  def name: String = s"(${lhs.name} $op ${rhs.name})"
}

case class BinaryOpFixedNamedQty(op: String, lhs: NamedQty, rhs: NamedQty, qty: FixedQty) extends NamedQtyFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(lhs.tree, rhs.tree), qty)

  def withTaint(taint: Taint): FixedQty = copy(qty = qty.withTaint(taint))

  def name: String = s"(${lhs.name} $op ${rhs.name})"
}

trait FunctionNamedQtyBase {
  def func: String
  def components: Seq[NamedQty]
  def renderInline: Boolean

  def name: String = {
    val componentsStr = components.map(_.name).mkString(", ")
    if (renderInline)
      s"$func($componentsStr)"
    else
      s"$func(${components.size} values)"
  }
}

case class FunctionNamedQty(func: String, renderInline: Boolean, components: Seq[NamedQty], qty: Qty)
  extends NamedQtyNotFixed with FunctionNamedQtyBase {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, components.map(_.tree), qty.toFixedQty())

  def withTaint(taint: Taint): Qty = copy(qty = qty.withTaint(taint))
}

case class FunctionFixedNamedQty(func: String, renderInline: Boolean, components: Seq[NamedQty], qty: FixedQty)
  extends NamedQtyFixed with FunctionNamedQtyBase {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, components.map(_.tree), qty)

  def withTaint(taint: Taint): FixedQty = copy(qty = qty.withTaint(taint))
}

case class NegateNamedQty(namedQty: NamedQty, qty: Qty) extends NamedQtyNotFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty.toFixedQty())

  def withTaint(taint: Taint): Qty = copy(qty = qty.withTaint(taint))

  def name: String = s"-${namedQty.name}"
}

case class NegateFixedNamedQty(namedQty: NamedQty, qty: FixedQty) extends NamedQtyFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty)

  def withTaint(taint: Taint): FixedQty = copy(qty = qty.withTaint(taint))

  def name: String = s"-${namedQty.name}"
}

case class InvertNamedQty(namedQty: NamedQty, qty: Qty) extends NamedQtyNotFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty.toFixedQty())

  def withTaint(taint: Taint): Qty = copy(qty = qty.withTaint(taint))

  def name: String = s"(1/${namedQty.name})"
}

case class InvertFixedNamedQty(namedQty: NamedQty, qty: FixedQty) extends NamedQtyFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty)

  def withTaint(taint: Taint): FixedQty = copy(qty = qty.withTaint(taint))

  def name: String = s"(1/${namedQty.name})"
}

case class RoundedNamedQty(nDP: Int, namedQty: NamedQty, qty: FixedQty) extends NamedQtyFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty)

  def withTaint(taint: Taint): FixedQty = copy(qty = qty.withTaint(taint))

  def name: String = s"Round(${namedQty.name}, $nDP)"
}

case class SignificantFiguresQty(nSF: Int, namedQty: NamedQty, qty: FixedQty) extends NamedQtyFixed {
  def tree: NamedQtyTreeNode = NamedQtyTreeNode(name, Seq(namedQty.tree), qty)

  def withTaint(taint: Taint): FixedQty = copy(qty = qty.withTaint(taint))

  def name: String = s"SignificantFigures(${namedQty.name}, $nSF)"
}

object Qty {
  type Percent = FixedQty
  type Price = FixedQty
  type Money = FixedQty
  type FreightPrice = FixedQty

  val NULL: FixedQty = apply(0, UOM.NULL, Taint.constant)
  val One: FixedQty = apply(1, UOM.SCALAR, Taint.constant)
  val Zero: FixedQty = apply(0, UOM.SCALAR, Taint.constant)

  def apply(value: Int, uom: UOM): SDQty = apply(value, uom, Taint.clean)

  def apply(value: Int, uom: UOM, taint: Taint): SDQty = apply(SmallDecimal.of(value), uom, taint)

  def apply(value: Double, uom: UOM): DblQty = apply(value, uom, Taint.clean)

  def apply(value: Double, uom: UOM, taint: Taint): DblQty = new DblQty(value, uom, taint)

  def apply(value: String, uom: UOM): FixedQty= apply(value, uom, Taint.clean)

  def apply(value: String, uom: UOM, taint: Taint): FixedQty= {
    val bd = BigDecimal(value)
    SmallDecimal.maybe(bd).map(apply(_, uom, taint)).getOrElse(apply(bd, uom, taint))
  }

  def apply(value: SmallDecimal, uom: UOM): SDQty = apply(value, uom, Taint.clean)

  def apply(value: SmallDecimal, uom: UOM, taint: Taint): SDQty = new SDQty(value, uom, taint)

  def apply(value: BigDecimal, uom: UOM): FixedQty = apply(value, uom, Taint.clean)

  def apply(value: BigDecimal, uom: UOM, taint: Taint): FixedQty = {
    SmallDecimal.maybe(value).map(Qty(_, uom, taint)).getOrElse(new BDQty(value, uom, taint))
  }

  def average(qtys: Traversable[Qty]): Qty = qtys.size match {
    case 0 => sys.error("Can't get the average of zero quantities.")
    case 1 => qtys.toSeq.head
    case _ => sum(qtys) / Qty(qtys.size, SCALAR, Taint.combine(qtys.map(_.taint)))
  }

  def sum(qtys: Traversable[Qty], zeroQty: Qty): Qty = {
    qtys.foldLeft[Qty](zeroQty)(_+_)
  }

  def sum(qtys: Traversable[Qty]): Qty = {
    sum(qtys, NULL)
  }

  def max(qtys: Traversable[Qty]): Qty = {
    if (qtys.isEmpty)
      throw new UnsupportedOperationException("Can't max empty sequence")
    // not qtys.max so we get an explanation
    qtys.reduceLeft[Qty](_ max _)
  }

  def min(qtys: Traversable[Qty]): Qty = {
    if (qtys.isEmpty)
      throw new UnsupportedOperationException("Can't min empty sequence")
    // not qtys.min so we get an explanation
    qtys.reduceLeft[Qty](_ min _)
  }

  /**
    * Sum only if qtys is a Seq of all Some(qty) with no Nones 
    */
  def sumIfAllDefined(qtys: Seq[Option[Qty]]): Option[Qty] = {
    val flattenQtys = qtys.flatten
    if(flattenQtys.size == qtys.size)
      Some(Qty.sum(flattenQtys))
    else
      None
  }

  def unapply(str: String): Option[FixedQty] = fromString(str).toOption
  
  private val QtyRegex = """([\-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)(.*)""".r
  def fromString(str: String): Either[TopazFail, FixedQty] = str.trim.replaceAll(",", "") match {
    case QtyRegex(valueText, uomText) =>
      UOM.fromString(uomText) match {
        case Right(uom) =>
          Right(Qty(valueText.replace(",", ""), uom, Taint.clean))
        case Left(_) =>
          TopazFail(s"Units '${uomText.trim}' are not valid")
      }
    case _ =>
      TopazFail(s"Quantity '$str' is malformed")
  }

  // we don't have a double to scalar to avoid people accidentally using a double like .1
  // and losing precision.
  // if you want to divide by a scalar double then write it out long form.
  implicit def intToScalarQty(value: Int): FixedQty = Qty(value, SCALAR, Taint.clean)

  implicit def smallDecimalToScalarQty(value: SmallDecimal): FixedQty = Qty(value, SCALAR, Taint.clean)

  implicit def bigDecimalToScalarQty(value: BigDecimal): FixedQty = Qty(value, SCALAR, Taint.clean)

  implicit class RichIntQty(value: Int) {
    def apply(uom: UOM): FixedQty = Qty(value, uom, Taint.clean)

    def q: FixedQty = Qty(value, UOM.SCALAR, Taint.clean)
  }

  implicit class RichDblQty(value: Double) {
    def apply(uom: UOM): DblQty =
      sys.error("We don't implement this for good reason. You probably don't want 7.45(USD) being a DblQty. " +
        "If you do then create it explicitly using the Qty.apply method.")

    def q = Qty(value, UOM.SCALAR, Taint.clean)
  }

  implicit object QtyNumberlike extends Numberlike[Qty] {
    def add(x: Qty, y: Qty): Qty = x + y

    def divide(x: Qty, y: Qty): Qty = x / y

    def multiply(x: Qty, y: Qty): Qty = x * y

    def subtract(x: Qty, y: Qty): Qty = x - y

    def multiply(x: Qty, y: Double): Qty = x * Qty(y, UOM.SCALAR, Taint.clean)

    def divide(x: Qty, y: Double): Qty = x / Qty(y, UOM.SCALAR, Taint.clean)
  }

  implicit object QuantityIsNumeric extends Numeric[Qty] {
    def toDouble(x: Qty) = throw TopazCodingError("Can't convert Qty to double")
    def toFloat(x: Qty) = throw TopazCodingError("Can't convert Qty to float")
    def toLong(x: Qty) = throw TopazCodingError("Can't convert Qty to long")
    def toInt(x: Qty) = throw TopazCodingError("Can't convert Qty to int")

    def fromInt(x: Int) = Qty(x.toDouble, UOM.SCALAR, Taint.clean)
    def negate(x: Qty) = -x
    def times(x: Qty, y: Qty) = x * y
    def minus(x: Qty, y: Qty) = x - y
    def plus(x: Qty, y: Qty) = x + y
    def compare(p1: Qty, p2: Qty) = p1 compare p2

    override def zero = Qty.NULL
    override def one = Qty.One
  }
  
  implicit class MaybeQty(q: Either[TopazFail, Qty]) {
    def named(name: => String)(implicit namingEnabled: NamingEnabled): Either[TopazFail, Qty] = {
      q.right.map(_.named(name))
    }
    def functionNamed(name: => String, renderInline: Boolean, components: => Seq[Qty])
      (implicit namingEnabled: NamingEnabled): Either[TopazFail, Qty] = {
      q.right.map(_.functionNamed(name, renderInline, components))
    }
  }
}

