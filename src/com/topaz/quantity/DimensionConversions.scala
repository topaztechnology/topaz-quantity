package com.topaz.quantity

import com.topaz.TopazCodingError
import com.topaz.quantity.UOM._
import com.topaz.utils.{Cache, CollectionUtils, EitherPimpsMixin, StringUtils, TopazFail}


case class TopazConversionFail(s: String) extends TopazFail

/**
 * Given a Map of rates like MT -> BBL = 7.5 we can convert to and from any subtype of MT or BBL.
 *
 * For example you can ask for the conversion from GAL to KG.
 *
 */
case class DimensionConversions(private val _rates: Map[(UOM, UOM), Either[TopazConversionFail, BigDecimal]])
  extends EitherPimpsMixin {
  
  lazy val cacheKey: String = DimensionConversions.cacheKey(_rates)
  
  def isEmpty = _rates.isEmpty
  
  def originalFormatRates = _rates

  private def ensureValidInput(uomPairs: Iterable[(UOM, UOM)]): Either[TopazFail, Unit] = {
    TopazFail.flatMapFromException {
      val basePairs = uomPairs.groupBy {
        case (from, to) => Set(from.asSingleUOM.base, to.asSingleUOM.base)
      }.toSeq
      firstToFail(basePairs) {
        case (pair, uoms) =>
          if (pair.size != 2)
            TopazFail(s"Can't use conversion for $uoms as they have the same base")
          else if (uoms.size != 1)
            TopazFail(s"Provided multiple conversions across the same bases $uoms")
          else
            Right(Unit)
      }
    }
  }

  ensureValidInput(_rates.keys).getOrThrow()

  private[quantity] val baseConversionRates: Map[(SingleUOM, SingleUOM), Either[TopazConversionFail, BigDecimal]] = _rates.map {
    case ((_from, _to), conv) =>
      val from = _from.asSingleUOM
      val to = _to.asSingleUOM
      val fromBase = from.base
      val toBase = to.base
      val newConv = conv.map {
        _ * to.conversionToBase / from.conversionToBase
      }
      (fromBase -> toBase) -> newConv
  }

  def ++(other: DimensionConversions): Either[TopazFail, DimensionConversions] = {
    val combinedUomPairs = _rates.keys.toSeq ++ other._rates.keys.toSeq
    ensureValidInput(combinedUomPairs).map(_ => DimensionConversions(_rates ++ other._rates))
  }

  def +(from: UOM, to: UOM, rate: BigDecimal): Either[TopazFail, DimensionConversions] = {
    val combinedUomPairs = _rates.keys.toSeq :+ (from, to)
    ensureValidInput(combinedUomPairs).map(_ => DimensionConversions(_rates + ((from, to) -> Right(rate))))
  }

  override def toString: String = {
    val ratesStr = _rates.map {
      case ((a, b), Left(fail)) => s"$a -> $b = ${fail.s}"
      case ((a, b), Right(conv)) => s"$a -> $b = $conv"
    }.toSeq.mkString("; ")
    s"DimensionConversions($ratesStr)"
  }

  def toPersistentString: String = {
    _rates.map {
      case ((a, b), Right(conv)) =>
        s"1 $a = ${conv.bigDecimal.toPlainString} $b"
      case _ =>
        throw TopazCodingError("Shouldn't be trying to serialise a DimensionConversions with errors")
    }.toSeq.mkString("; ")
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: DimensionConversions => chainedRates == other.chainedRates
    case _ => false
  }

  override def hashCode(): Int = chainedRates.hashCode()

  private[quantity] val chainedRates: Map[(SingleUOM, SingleUOM), Either[TopazConversionFail, BigDecimal]] = {
    // Expands the supplied rates to include all possible chains
    var rates = Map[(SingleUOM, SingleUOM), Either[TopazConversionFail, BigDecimal]]()
    var more: Map[(SingleUOM, SingleUOM), Either[TopazConversionFail, BigDecimal]] = baseConversionRates ++ baseConversionRates.map {
      case ((from, to), Right(rate)) => ((to, from), Right(com.topaz.quantity.QtyUtils.BDOne / rate))
      case ((from, to), left) => ((to, from), left)
    }
    while (more.nonEmpty) {
      rates ++= more
      more = for {
        ((from1, to1), rate1) <- rates
        ((from2, to2), rate2) <- rates
        if to1 == from2 && from1 != to2
      } yield {
        val conversion = for {
          r1 <- rate1
          r2 <- rate2
        } yield {
          val conversion = r1 * r2 * to1.conversionToBase / from2.conversionToBase
          val existingConversion = rates.get((from1, to2))
          existingConversion.foreach {
            case Right(c) =>
              if ((c - conversion).abs > 1e-9)
                throw new IllegalStateException("Inconsistent conversions: " +(from1, to2))
            case Left(error) =>
              throw new IllegalStateException("Inconsistent conversions: " +(from1, to2))
          }
          conversion
        }
        (from1, to2) -> conversion
      }
      more = more.filterNot {
        case ((uom1, uom2), _) => rates.contains((uom1, uom2))
      }
    }
    rates
  }

  /**
    * Converts `from` to `to`. Both UOMs need to be simple single units. For example
    * BBL, GAL, MT, G^-2 are all fine.
    * BBLUSD, G/USD will not work.
    */
  def rate(from: SingleUOM, to: SingleUOM): Either[TopazConversionFail, BigDecimal] = if(from.power == to.power) {
    val fromBase = from.base
    val toBase = to.base

    def fromMap(a: SingleUOM, b: SingleUOM): Either[TopazConversionFail, BigDecimal] = chainedRates.get((a, b)) match {
      case Some(Right(c)) => Right(c)
      case Some(Left(e)) => Left(e)
      case _ => Left(TopazConversionFail(s"No conversion from $a to $b"))
    }  
    
    val fromToRate = fromMap(fromBase, toBase)
    val toFromRate = fromMap(toBase, fromBase)
    val rescale = from.conversionToBase / to.conversionToBase
    val invRescale = to.conversionToBase / from.conversionToBase

    // this looks a little strange but the idea is not to invert twice. e.g. if you have a Map of rates
    // like MT -> BBL = 7.5 and you ask to convert BBL^-1 -> MT^-1 we should return 7.5 and not
    // 1 / (1 / 7.5) (which would give you 7.500000000000000000000000000000002)
    if(from.power < 0) {
      toFromRate.map(_ / rescale).orElse(fromToRate.map(r => 1 / (r * rescale))).map(_.pow(from.power.abs))
    } else {
      fromToRate.map(_ * rescale).orElse(toFromRate.map(r => 1 / (r * invRescale))).map(_.pow(from.power))
    }
  } else {
    Left(TopazConversionFail(s"Can't convert from $from to $to, powers differ."))
  }
}

object DimensionConversions extends CollectionUtils {
  val empty = new DimensionConversions(Map.empty)

  private val cache = Cache.createStaticCache("DimensionConversions")

  def commonConversions(convs: Seq[DimensionConversions]): DimensionConversions = {
    convs.size match {
      case 0 => DimensionConversions.empty
      case 1 => convs.head
      case _ =>
        val allKeys = convs.flatMap(_._rates.keySet).distinctBy {
          case (a, b) => Set(a.asSingleUOM.base, b.asSingleUOM.base)
        }
        val keysAndConversions = allKeys.map {
          case k@(u1, u2) => k -> convs.map(c => u1.in(u2, c).left.map(f => TopazConversionFail(f.s))).distinct
        }
        val keysWithMatchingConversions: Map[(UOM, UOM), Either[TopazConversionFail, BigDecimal]] = keysAndConversions.collect {
          case (k, conversions) if conversions.size == 1 => k -> conversions.head
        }(collection.breakOut)

        new DimensionConversions(keysWithMatchingConversions)
    }
  }

  private def cacheKey[V](rates: Map[(UOM, UOM), V]) = {
    rates.map {
      case ((f, t), v) => s"$f$t${v}"
    }.mkString(",")
  }

  def fromMap(rates: Map[(UOM, UOM), BigDecimal]): Either[TopazFail, DimensionConversions] = {
    cache.memoize(cacheKey(rates)) {
      try {
        Right(new DimensionConversions(rates.map { case (k, v) => k -> Right(v) }))
      } catch {
        case (ex: Exception) => TopazFail(s"Error found in conversions: ${ex.getMessage}")
      }
    }
  }

  def fromConversionFactor(rate: BigDecimal): DimensionConversions =
    fromDensityAndConversionFactor(density = None, conversionFactor = Some(rate))

  def fromDensity(rate: BigDecimal): DimensionConversions =
    fromDensityAndConversionFactor(density = Some(rate), conversionFactor = None)

  def fromDensityAndConversionFactor(density: Option[BigDecimal], conversionFactor: Option[BigDecimal]): DimensionConversions = {
    val map = Map.empty[(UOM, UOM), Either[TopazConversionFail, BigDecimal]] +?
      density.map(d => (M3, MT) -> Right(d)) +?
      conversionFactor.map(c => (MT, BBL) -> Right(c))

    if (map.isEmpty) {
      empty
    } else {
      cache.memoize(cacheKey(map))(new DimensionConversions(map))
    }
  }

  /**
    * Parses a list of semicolon-delimited conversions into a DimensionConversions object.
    * Example input: "1 m3=0.789 MT; 1 m3=5.9964 bbl".
    *
    * For the benefit of the users, we roll together and return as many errors as we can,
    * rather than just returning the first error encountered.
    */
  def fromString(string: String): Either[TopazFail, DimensionConversions] = {
    if (string == null || string.trim.length == 0)
      Right(DimensionConversions.empty)
    else {
      import StringUtils.localNewline

      def convertTerm(term: String): Either[TopazFail, ((UOM, UOM), BigDecimal)] = {
        term.split("=").map(_.trim) match {
          case Array(lhs, rhs) =>
            Seq(
              Qty.fromString(lhs).flatMap { qty =>
                if (qty.bdValue != 1)
                  TopazFail(s"Each term must begin with '1'")
                else
                  Right(qty)
              },
              Qty.fromString(rhs).flatMap { qty =>
                if (qty.bdValue == 0)
                  TopazFail(s"A term's conversion rate cannot be zero")
                else
                  Right(qty)
              }
            ) match {
              case Seq(Right(lhsQty), Right(rhsQty)) =>
                Right((lhsQty.uom, rhsQty.uom) -> rhsQty.bdValue)
              case other =>
                val errors = other.flatMap(_.left.toOption.map(_.s))
                TopazFail(s"""Term '$term' has errors:${errors.mkString(localNewline, localNewline, "")}""")
            }

          case _ => TopazFail(s"Term '$term' is malformed")
        }
      }

      val terms = string.split(";").map(_.trim)
      val convertedTerms = terms.map { convertTerm }
      val fails = convertedTerms.flatMap(_.left.toOption.map(_.s))

      if (fails.isEmpty)
        DimensionConversions.fromMap(convertedTerms.map(_.get).toMap)
      else
        TopazFail(fails.mkString(localNewline))
    }
  }

}
