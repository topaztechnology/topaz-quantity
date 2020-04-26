package com.topaz.quantity

import com.topaz.TopazCodingError
import com.topaz.quantity.UOM.SingleUOM
import com.topaz.utils._
import com.topaz.utils.LongUtils._
import org.apache.commons.math3.util.ArithmeticUtils
import com.topaz.quantity.DimensionConversions._

import scala.math.BigDecimal
import scala.runtime.ScalaRunTime
import scalaz.Scalaz._

/**
 * @param category The category is a grouping for this unit. For example the unit kg would be in the category
 *                 'mass'. The unit us_cent would be in the category USD, as would the unit usd.
 *                 You can convert directly between units in the same categories. When you define a unit
 *                 you define it with the conversion to the base unit in that category. The base unit is assumed
 *                 to have a conversion of 1.
 *                 So if the base unit of category 'mass' is the gram. You would define kg as a member of the 'mass'
 *                 category with a conversion of 1000.
 *
 * @param unit     The unit itself, e.g. usd, us_cent, g, kg, lb, usd/bbl, g^2/pence.
 *
 * @see [[UOMRatio]] which explains how we represent the numerator and denominator as primes. In the case of the
 *     UOM kg/bbl, the category would be primes representing Mass/OilVolume and the unit would be kg/bbl.
 */
case class UOMPrimes private (category: UOMRatio, unit: UOMRatio) extends UOM with EitherPimpsMixin {

  override lazy val hashCode = ScalaRunTime._hashCode(this) /* Performance optimization */

  def addOrSubtract(other: UOM): Either[TopazFail, Option[BigDecimal]] = other match {
    case UOMPrimes(`category`, `unit`) => Right(None)
    case o@UOMPrimes(`category`, _) => Right(Some(magnitude / o.magnitude))
    case _:UOMPrimes =>
      TopazFail(s"Can't add [$this] and [$other]")
    case t => throw TopazCodingError("Invalid type: " + t.getClass)
  }

  def mult(_other: UOM): (UOM, BigDecimal) = {
    _other match {
      case other: UOMPrimes => UOMPrimes.multCache.memoize((this, other)) {
        val newUOM = UOMPrimes.buildUOM(category * other.category, unit * other.unit)
        val dimGCD = newUOM.category.gcd
        // if the category has a gcd > 1 then we have some redundancy in the UOM, something like
        // USDBBL/CENT. We need to reduce this to BBL
        if (dimGCD > 1) {
          // reduce the category component, e.g. MASSVOLUME/VOLUME => MASS
          val uomReducedCategories = newUOM.copy(category = newUOM.category.reduce)

          // now reduce the unit component, e.g. MTBBL/BBL => MT
          val redundantPrimes = newUOM.category.redundantPrimes
          val secondaryPrimes = redundantPrimes.flatMap(p => UOMPrimes.categoryToListOfUnits(p).map(u => u.unit.num.toInt))
          val numDivisible = secondaryPrimes.filter(p => newUOM.unit.num % p == 0)
          val denDivisible = secondaryPrimes.filter(p => newUOM.unit.den % p == 0)

          val uomReducedSecondary = numDivisible.zip(denDivisible).foldLeft(uomReducedCategories) {
            case (uom, (p1, p2)) =>
              val newRatio = UOMRatio(uom.unit.num / p1, uom.unit.den / p2)
              uom.copy(unit = newRatio)
          }

          (uomReducedSecondary.intern, newUOM.magnitude / uomReducedSecondary.magnitude)
        } else if (this.isScalarCategory && other.isScalarCategory) {
          (UOM.SCALAR, magnitude * other.magnitude)
        } else if (this.category == other.category.invert) {
          (UOM.SCALAR, magnitude / other.magnitude)
        } else if (this.isScalarCategory) {
          (other, this.magnitude)
        } else if (other.isScalarCategory) {
          (this, other.magnitude)
        } else {
          (newUOM, QtyUtils.BDOne)
        }
      }
      case t => throw TopazCodingError("Invalid type: " + t.getClass)
    }
  }

  def invert: UOMPrimes = UOMPrimes.invertCache.memoize(this) {
    UOMPrimes.buildUOM(category.invert, unit.invert)
  }

  def zero: FixedQty = FixedQty(0, this)

  def pow(n: Int): UOM = UOMPrimes.powCache.memoize(this, n) {
    val res = UOMPrimes.buildUOM(category.pow(n), unit.pow(n))
    res.intern
  }

  protected def magnitude: BigDecimal = {
    asPrimeMap.foldLeft(QtyUtils.BDOne) {
      case (bd, (prime, power)) => bd * UOMPrimes.unitConversionToCategoryBase(prime).pow(power)
    }
  }

  def in(_other: UOM, conversions: DimensionConversions = empty): Either[TopazFail, BigDecimal] = {
    def inOtherUom = {
      _other match {
        case other: UOMPrimes =>
          if (this == other) {
            Right(QtyUtils.BDOne)
          } else {

            (this.isScalarCategory, other.isScalarCategory) match {

              case (true, true) => Right(magnitude / other.magnitude)

              case (false, false) => {
                val thisUOMs = seqOfUOMs
                val otherUOMs = other.seqOfUOMs
                val conversionScalesFromTo: Seq[(UOM, UOM, scala.BigDecimal)] = thisUOMs.flatMap {
                  from =>
                    otherUOMs.flatMap(
                      to =>
                        from.conversionFactor(to, conversions).toOption.map((from.asUOM, to.asUOM, _))
                    ) match {
                      case conv :: Nil => Some(conv) // one conversion available between `from` and `otherUOMs`
                      case Nil => None // No conversions available. We'll fail on this.
                      case _ => None // Multiple conversions available. We'll also fail on this.
                    }
                }
                var acc: UOM = this
                val conversionScale = conversionScalesFromTo.foldLeft(QtyUtils.BDOne) {
                  case (scale, (f, t, s)) =>
                    acc = acc / f * t
                    s * scale
                }
                if (acc == other)
                  Right(conversionScale)
                else
                  TopazFail(s"Failed to convert $stringForErrors to ${other.stringForErrors}")
              }

              case _ =>
                TopazFail(s"Failed to convert $stringForErrors to ${other.stringForErrors}")
            }
          }

        case t => throw TopazCodingError("Invalid type: " + t.getClass)
      }
    }
    UOMPrimes.inOtherUOMCache.memoize((this, _other, conversions.cacheKey))(inOtherUom)
  }

  override def toString = string
  
  def stringForErrors: String = this match {
    case UOM.NULL => "NULL"
    case UOM.SCALAR => "Scalar"
    case o => o.toString
  }

  def isCcy = UOMPrimes.baseCurrenciesMap.contains(this)

  def isScalarCategory = category == UOMRatio(1, 1)

  lazy val numerator = {
    if (unit.factorNum.isEmpty)
      UOM.SCALAR
    else {
      unit.factorNum.groupBy(identity).map {
        case (prime, instances) => UOMPrimes.unitPrimeToUOM(prime).pow(instances.size)
      }.reduce(_ * _)
    }
  }

  lazy val denominator = invert.numerator

  def inMajorCcy: UOM = UOMPrimes.inBaseCcyCache.memoize(this){
    if (this == UOM.NULL || asUOMPowerMap.isEmpty)
      this
    else
      asUOMPowerMap.map {
        case (uom, pow) =>
          UOMPrimes.baseCurrenciesMap.getOrElse(uom, uom).pow(pow)
      }.reduce(_ * _)
  }

  override def allNames = {
    if (this == UOM.NULL) {
      UOM.NULL.toString :: Nil
    } else if (this == UOM.SCALAR) {
      UOM.SCALAR.toString :: Nil
    } else {
      val primes = asPrimeMap
      UOMPrimes.unitPrimeToSymbol(primes.head._1).map(stringWithPower(_, primes.head._2))
    }
  }

  private def stringWithPower(uomStr: String, power: Int) = {
    if (power == 1) {
      uomStr
    } else {
      uomStr +"^" + power
    }
  }

  private lazy val string: String = {
    def formatPower(reduced: UOMRatio, primes: List[Int], negate: Boolean) = {
      val strings: IndexedSeq[String] = reduced match {
        case UOMRatio(i, 1) if i < 2 => Vector(UOMPrimes.unitPrimeToSymbol(i.toInt).head) // special case for null and scalar
        case _ =>
          primes.groupBy(identity).map {
            case (prime, all) =>
              val power = all.size * (if (negate) -1 else 1)
              stringWithPower(UOMPrimes.symbolFor(prime), power)
          }(collection.breakOut)
      }
      CollectionUtils.substituteAll(UOM.wellKnownGroupings, strings)
    }

    // we only care about the unit type for string representation
    val reduced = unit.reduce
    val num = reduced.factorNum
    val den = reduced.factorDen
    val numString = formatPower(reduced, num, negate = false).mkString(" ")
    if (den.nonEmpty && numString.isEmpty) {
      formatPower(reduced, den, negate = true).mkString("")
    } else if (den.nonEmpty) {
      val denString = formatPower(reduced, den, negate = false).mkString(" ")
      if (isCcyPair)
        denString + numString
      else
        numString + "/" + denString
    } else {
      numString
    }
  }

  def categoryGrouping: AnyRef = {
    category
  }

  override protected def intern: UOMPrimes = {
    UOMPrimes.interningCache.memoize(this) {
      this
    }
  }

  def readResolve = {
    intern
  }

  override def asSingleUOM: UOM.SingleUOM = {
    UOMPrimes.SingleUOMPrimes(this)
  }

  private lazy val asPrimeMap: Map[Int, Int] = {
    val num = unit.factorNum.groupBy(identity).map { case (k, v) => k -> v.size }
    val den = unit.factorDen.groupBy(identity).map { case (k, v) => k -> v.size * -1 }
    num.mappend(den)
  }

  private def asUOMPowerMap: Map[UOMPrimes, Int] = asPrimeMap.mapKeys(UOMPrimes.unitPrimeToUOM)

  // Converts USD BBL^2 / GBP to Seq(USD, BBL^2, GBP^-1)
  private def seqOfUOMs: Seq[UOMPrimes.SingleUOMPrimes] = {
    asUOMPowerMap.map {case (uom, pow) => uom.pow(pow).asSingleUOM.asInstanceOf[UOMPrimes.SingleUOMPrimes]}.toSeq
  }
}

object UOMPrimes extends UOMBuilder {
  private val primesIterator = LimitedPrimeGenerator.primes
  private val inOtherUOMCache = Cache.createStaticCache("UOMPrimesCache.inOtherUOMCache")
  private val multCache = Cache.createStaticCache("UOMPrimesCache.mult")
  private val invertCache = Cache.createStaticCache("UOMPrimesCache.invert")
  private val powCache = Cache.createStaticCache("UOMPrimesCache.pow")
  private val interningCache = Cache.createStaticCache("UOMPrimesCache.intern")
  private val inBaseCcyCache = Cache.createStaticCache("UOMPrimesCache.inBaseCcy")

  private case class UOMData(category: Int, unit: Int, magnitude: BigDecimal, strs: List[String], uom: UOMPrimes)

  private class UomDataBuilder {
    private var uomData: List[UOMData] = Nil
    private var built = false

    def add(data: UOMData) = this.synchronized {
      require(!built, "UOMs already built, can't be added to now. " + data.strs.mkString(","))
      uomData ::= data
    }

    def apply() = this.synchronized {
      built = true
      uomData
    }
  }
  private val uomData: UomDataBuilder = new UomDataBuilder

  override def build(category: FixedConversionCategory, relationToBase: scala.BigDecimal, names: String*):  UOMPrimes = {
    UOMPrimes.synchronized {
      val prime = primesIterator.next()
      val uom = buildUOM(UOMRatio(category.prime, 1), UOMRatio(prime, 1))
      val data = UOMData(category.prime, prime, relationToBase, names.toList, uom)
      uomData.add(data)
      uom
    }
  }

  override val Null: UOM = apply(0, 0, "NULL")

  override val Scalar: UOM = apply(1, 1, "")

  /*
     * All UOM construction should be done through this method
     * to guarantee interning
     */
  def buildUOM(category: UOMRatio, unit: UOMRatio): UOMPrimes = {
    UOMPrimes(category, unit).intern
  }

  private def apply(category: Int, unit: Int, str: String) = UOMPrimes.synchronized {
    val uom = buildUOM(UOMRatio(category, 1), UOMRatio(unit, 1))
    val data = UOMData(category, unit, 1.0, str :: Nil, uom)
    uomData.add(data)
    uom
  }

  private def symbolFor(prime: Int) = unitPrimeToSymbol(prime).head

  private lazy val instances: Map[UOMPrimes, UOMPrimes] = uomData().map(d => d.uom -> d.uom).toMap // used for interning.
  private lazy val categoryToListOfUnits: Map[Int, List[UOMPrimes]] = {
    uomData().map(_.uom).groupBy(_.category.num.toInt)
  }
  private lazy val unitPrimeToSymbol: Map[Int, List[String]] = uomData().map(d => d.unit -> d.strs).toMap
  lazy val unitPrimeToUOM: Map[Int, UOMPrimes] = uomData().map(d => d.unit -> d.uom).toMap
  private lazy val unitConversionToCategoryBase: Map[Int, scala.BigDecimal] = {
    uomData().map(d => d.unit -> d.magnitude).toMap
  }

  private[quantity] def knownBaseCurrencies(): Seq[UOM] = baseCurrenciesMap.keys.toVector.map(_.inMajorCcy).distinct

  private lazy val baseCurrenciesMap: Map[UOMPrimes, UOMPrimes] = { // USD -> US_CENT, USD -> USD, etc.
    val (ccyBase, ccyDenominations) = instances.keys.flatMap {
      uom =>
        if(FixedConversionCategory.currencyPrimes.contains(uom.category.num.toInt))
           Some(uom)
        else
          None
    }.partition(_.magnitude == 1)

    ccyDenominations.map {
      uom => uom -> ccyBase.find(_.category == uom.category).getOrElse(sys.error("No base unit for " + uom))
    } ++ ccyBase.map(ccy => ccy -> ccy)
  }.toMap

  private case class SingleUOMPrimes(uom: UOMPrimes) extends SingleUOM with EitherPimpsMixin {
    require(uom.asPrimeMap.size == 1, "Should only be a single unit: " + uom)

    lazy val (secondaryPrime: Int, power: Int) = {
      uom.asPrimeMap.head
    }

    def asUOM = uom
    lazy val base: SingleUOM = {
      val prime = if (uom.category.num == 1)
        uom.category.factorDen.head
      else
        uom.category.factorNum.head

      val base = UOMPrimes.categoryToListOfUnits(prime).find(_.magnitude == QtyUtils.BDOne).getOrElse(
        throw TopazCodingError(s"$this has no baseUOM")
      )
      base.asSingleUOM
    }

    lazy val conversionToBase: scala.BigDecimal = unitConversionToCategoryBase(secondaryPrime)

    private def scaleBetween(other: SingleUOM): Either[TopazFail, BigDecimal] = {
      if(base == other.base && power == other.power) {
        Right((conversionToBase / other.conversionToBase).pow(power))
      } else {
        TopazFail(s"Can't convert from $this to $other")
      }
    }
    
    def conversionFactor(to: UOM.SingleUOM, conversions: DimensionConversions): Either[TopazFail, BigDecimal] = {
      if (this == to) {
        Right(QtyUtils.BDOne)
      } else if (this.power == to.power) {
        this.scaleBetween(to).orElse(conversions.rate(this, to))
      } else {
        TopazFail(s"Can't convert from $this to $to")
      }
    }

    override def toString: String = uom.toString
  }
}

/*
 * Based on prime factorisation
 * If we want to represent BBL^2USD, and BBL => 3, USD => 5, we have 3 * 3 * 5 => 45
 * 45 can only be factored back into 3,3,5 (using primes) so we can recover the unit.
 * For BBl^2/USD we set num => 3*3, den => 5
*/
case class UOMRatio(num: Long, den: Long) {

  def redundantPrimes: List[Int] = if (num == 0) {
    Nil
  } else {
    var gcd = ArithmeticUtils.gcd(num, den)
    var reduced = this
    var primes = List[Int]()
    while (gcd > 1) {
      reduced = copy(num / gcd, den / gcd)
      primes :::= LimitedPrimeGenerator.factor(gcd)
      gcd = ArithmeticUtils.gcd(reduced.num, reduced.den)
    }
    primes
  }

  // reduces something like USDBBL/USD to BBL
  def reduce: UOMRatio = redundantPrimes.foldLeft(this)((u, p) => u.copy(u.num / p, u.den / p))

  def gcd: Long = ArithmeticUtils.gcd(num, den)

  def *(other: UOMRatio): UOMRatio = UOMRatio(num * other.num, den * other.den)

  def /(other: UOMRatio): UOMRatio = this * other.invert

  def invert: UOMRatio = UOMRatio(den, num)

  def pow(n: Int): UOMRatio = {
    n match {
      case 0 => UOMRatio(1, 1)
      case n if n < 0 => UOMRatio(num ** n.abs, den).invert
      case n if n > 0 => UOMRatio(num ** n, den)
    }
  }

  def factorNum: List[Int] = if (num <= 1) Nil else LimitedPrimeGenerator.factor(num)

  def factorDen: List[Int] = if (den <= 1) Nil else LimitedPrimeGenerator.factor(den)
}

