package com.topaz.quantity

import com.topaz.TopazCodingError
import com.topaz.utils._
import scala.math.BigDecimal
import scala.util.Either
import com.topaz.quantity.UOM.WSC


trait UOM {
  /**
   * @return Right(bd) is the value to divide the second component by.
   *         e.g. if you had USD.add(USC) you would be returned 100.0
   *         Right(None) if no scale needs to be applied
   *         Left(error) if this is not a valid addition
   */
  def addOrSubtract(other: UOM): Either[TopazFail, Option[scala.BigDecimal]]

  /**
   * @return The new UOM and the scale to apply to the multiplication.
   */
  def mult(other: UOM): (UOM, scala.BigDecimal)

  def div(other: UOM): (UOM, scala.BigDecimal) = mult(other.invert)

  /**
   * Useful for defining UOMs like USD*USD
   */
  def *(other: UOM): UOM = {
    val (uom, scale) = mult(other)
    TopazCodingError.require(scale == QtyUtils.BDOne,
      f"Can't use '*' syntax with UOMs whose scale isn't 1 - got $this, $other")
    uom
  }

  /**
     * Useful for defining UOMs like USD/BBL
     */
  def /(other: UOM): UOM = {
    val (uom, scale) = div(other)
    TopazCodingError.require(scale == QtyUtils.BDOne,
      f"Can't use '/' syntax with UOMs whose scale isn't 1 - got $this, $other")
    uom
  }

  def invert: UOM

  def zero: FixedQty

  def in(other: UOM, conversions: DimensionConversions = DimensionConversions.empty): Either[TopazFail, BigDecimal]
  
  def hasConstantConversion(to: UOM): Boolean = constantConversion(to).isRight
  
  def constantConversion(to: UOM): Either[TopazFail, BigDecimal] = in(to, conversions = DimensionConversions.empty)

  def toString: String

  def isCcy: Boolean

  def isMajorCCY: Boolean = isCcy && this == inMajorCcy

  def isCcyPair: Boolean = numerator.isCcy && denominator.isCcy

  def isNull: Boolean = this == UOM.NULL

  def numerator: UOM

  def priceMajorCCY: UOM = {
    require(numerator.isCcy || numerator == WSC, f"$this doesn't appear to be a price")
    numerator.inMajorCcy
  }

  /**
    * ccy1 and ccy2 are FX trading standard terms.
    * Note that the unit CCY2/CCY1 is written CCY1CCY2
    */
  def ccy1: UOM = {
    require(isCcyPair, f"$this is not a currency pair")
    denominator.inMajorCcy
  }

  def ccy2: UOM = {
    require(isCcyPair, f"$this is not a currency pair")
    numerator.inMajorCcy
  }

  def denominator: UOM

  def inMajorCcy: UOM

  def pow(n: Int): UOM

  def allNames: Seq[String]

  def isScalarCategory: Boolean

  def asSingleUOM: UOM.SingleUOM

  def requireIsCurrency(): Unit = {
    require(isCcy, s"$this is not a currency")
  }

  def isCcyOrFail(): Either[TopazFail, UOM] = {
    if (isCcy) Right(this) else TopazFail(s"$this is not a currency")
  }

  /**
    * Used to group together UOMs that can be added.
    * What's returned is not important as long as it has hashCode
    * and equals implemented.
    */
  def categoryGrouping: AnyRef

  protected def intern: UOM
}

/**
 * Since we can have different implementations of UOM this provides the UOM object
 * with the means to create instances in the desired implementation.
 */
trait UOMBuilder {
  def build(category: FixedConversionCategory, relationToBase: BigDecimal, names: String*): UOM
  def Null: UOM
  def Scalar: UOM
}

object UOM extends EitherPimpsMixin {

  // If you add a new UOM with a unicode string (like ¢) this regex will need the unicode
  // character added to it (as \w doesn't match unicode characters).
  // Also make the same change to `ProductOfUOMTerms` - the duplication sucks but
  // tricky to avoid as the latter uses non-capturing groups.
  private val UomTermRegex = """([HK] )?([\w¢]+)(\^-?\d+)?""".r

  private val ProductOfUOMTerms = {
    val UomTerm = """[\w¢]+(?:\^-?\d+)?"""
    s"""\\s*$UomTerm(?:\\s+$UomTerm)*\\s*"""
  }

  val Regex = s"""(($ProductOfUOMTerms)/($ProductOfUOMTerms))|($ProductOfUOMTerms)|([A-Z]{6})""".r

  private val fromStringCache = Cache.createStaticCache("UOM.fromString")
  private val multCache = Cache.createStaticCache("UOM.mult")

  private implicit val builder: UOMBuilder = UOMPrimes

  def unapply(uomStr: String): Option[UOM] = fromStringCache.memoize(uomStr) {
    uomStr.trim match {
      case "" => Some(SCALAR)
      case "%" => Some(PERCENT)
      case FromFXString(fx) => Some(fx)
      case FromFractionalString(uom) => Some(uom)
      case FromNonFractionalString(uom) => Some(uom)
      case _ => None
    }
  }

  def fromString(uomStr: String): Either[TopazFail, UOM] = {
    uomStr match {
      case UOM(uom) => Right(uom)
      case _ => TopazFail(s"Can't convert '$uomStr' to UOM")
    }
  }

  def fromStringOrThrow(uomStr: String): UOM = {
    fromString(uomStr) match {
      case Right(uom) => uom
      case Left(error) => throw TopazCodingError(s"Not a valid uom '$uomStr'")
    }
  }

  def knownBaseCurrencies(): Seq[UOM] = UOMPrimes.knownBaseCurrencies()

  private object FromFractionalString {

    private val Regex = s"""($ProductOfUOMTerms)/($ProductOfUOMTerms)""".r

    def unapply(uomText: String): Option[UOM] = uomText match {
      case Regex(numeratorText, denominatorText) =>
        (numeratorText, denominatorText) match {
          case (FromNonFractionalString(numerator), FromNonFractionalString(denominator)) =>
            Some(numerator / denominator)
          case _ => None
        }
      case _ => None
    }

    def apply(uomText: String) = uomText match {
      case FromFractionalString(uom) => uom
      case _ => throw new TopazCodingError(s"Can't convert $uomText to UOM")
    }
  }

  private object FromNonFractionalString {

    def apply(uomText: String) = uomText match {
      case FromNonFractionalString(uom) => uom
      case _ => throw new TopazCodingError(s"Can't convert $uomText to UOM")
    }

    def unapply(uomText: String): Option[UOM] = if (uomText.trim matches ProductOfUOMTerms) {
      val possibleTerms = UomTermRegex.findAllIn(uomText.trim).toVector
      val terms = possibleTerms.flatMap {
        uomTerm: String => uomTerm match {
          case UomTermRegex(maybeScale, symbolName, maybePower) =>
            // maybe scale is K or H in kbbl or HMT
            val power = Option(maybePower).flatMap{p => ParseInt.unapply(p.drop(1))}.getOrElse(1)
            val fullNamne = Option(maybeScale).map(s => s + symbolName).getOrElse(symbolName)
            fromName(fullNamne).map(_.pow(power))
        }
      }
      if (terms.nonEmpty && terms.size == possibleTerms.size)
        Some(terms.reduce(_ * _))
      else
        None
    } else {
      None
    }
  }

  private object FromFXString {

    def unapply(fx: String): Option[UOM] = {
      if(fx.length == 6)
        (fx.substring(0, 3), fx.substring(3, 6)) match {
          case (UOM(from), UOM(to)) => Some(to / from)
          case _ => None
        }
      else
        None
    }
  }

  val NULL = builder.Null
  val SCALAR = builder.Scalar
  val PERCENT = UOM(FixedConversionCategory.SCALAR, 0.01, "%")
  val IMM_PRICE_POINT = UOM(FixedConversionCategory.SCALAR, 0.01, "pp")
  val BASIS_POINT = UOM(FixedConversionCategory.SCALAR, 0.0001, "bp")
  val WSC = UOM(FixedConversionCategory.SCALAR, 0.01, "WSC") // Worldscale for freight pricing

  // ccys
  val AED = UOM(FixedConversionCategory.AED, 1.0, "AED") // United Arab Emirates Dirham
  val AFN = UOM(FixedConversionCategory.AFN, 1.0, "AFN") // Afghanistan Afghani
  val ALL = UOM(FixedConversionCategory.ALL, 1.0, "ALL") // Albania Lek
  val AMD = UOM(FixedConversionCategory.AMD, 1.0, "AMD") // Armenia Dram
  val ANG = UOM(FixedConversionCategory.ANG, 1.0, "ANG") // Netherlands Antilles Guilder
  val AOA = UOM(FixedConversionCategory.AOA, 1.0, "AOA") // Angola Kwanza
  val ARS = UOM(FixedConversionCategory.ARS, 1.0, "ARS") // Argentina Peso
  val AUD = UOM(FixedConversionCategory.AUD, 1.0, "AUD") // Australia Dollar
  val AWG = UOM(FixedConversionCategory.AWG, 1.0, "AWG") // Aruba Guilder
  val AZN = UOM(FixedConversionCategory.AZN, 1.0, "AZN") // Azerbaijan New Manat
  val BAM = UOM(FixedConversionCategory.BAM, 1.0, "BAM") // Bosnia and Herzegovina Convertible Marka
  val BBD = UOM(FixedConversionCategory.BBD, 1.0, "BBD") // Barbados Dollar
  val BDT = UOM(FixedConversionCategory.BDT, 1.0, "BDT") // Bangladesh Taka
  val BGN = UOM(FixedConversionCategory.BGN, 1.0, "BGN") // Bulgaria Lev
  val BHD = UOM(FixedConversionCategory.BHD, 1.0, "BHD") // Bahrain Dinar
  val BIF = UOM(FixedConversionCategory.BIF, 1.0, "BIF") // Burundi Franc
  val BMD = UOM(FixedConversionCategory.BMD, 1.0, "BMD") // Bermuda Dollar
  val BND = UOM(FixedConversionCategory.BND, 1.0, "BND") // Brunei Darussalam Dollar
  val BOB = UOM(FixedConversionCategory.BOB, 1.0, "BOB") // Bolivia Boliviano
  val BRL = UOM(FixedConversionCategory.BRL, 1.0, "BRL") // Brazil Real
  val BSD = UOM(FixedConversionCategory.BSD, 1.0, "BSD") // Bahamas Dollar
  val BTN = UOM(FixedConversionCategory.BTN, 1.0, "BTN") // Bhutan Ngultrum
  val BWP = UOM(FixedConversionCategory.BWP, 1.0, "BWP") // Botswana Pula
  val BYR = UOM(FixedConversionCategory.BYR, 1.0, "BYR") // Belarus Ruble
  val BZD = UOM(FixedConversionCategory.BZD, 1.0, "BZD") // Belize Dollar
  val CAD = UOM(FixedConversionCategory.CAD, 1.0, "CAD") // Canada Dollar
  val CDF = UOM(FixedConversionCategory.CDF, 1.0, "CDF") // Congo/Kinshasa Franc
  val CHF = UOM(FixedConversionCategory.CHF, 1.0, "CHF") // Switzerland Franc
  val CLP = UOM(FixedConversionCategory.CLP, 1.0, "CLP") // Chile Peso
  val CNY = UOM(FixedConversionCategory.CNY, 1.0, "CNY") // China Yuan Renminbi
  val COP = UOM(FixedConversionCategory.COP, 1.0, "COP") // Colombia Peso
  val CRC = UOM(FixedConversionCategory.CRC, 1.0, "CRC") // Costa Rica Colon
  val CUC = UOM(FixedConversionCategory.CUC, 1.0, "CUC") // Cuba Convertible Peso
  val CUP = UOM(FixedConversionCategory.CUP, 1.0, "CUP") // Cuba Peso
  val CVE = UOM(FixedConversionCategory.CVE, 1.0, "CVE") // Cape Verde Escudo
  val CZK = UOM(FixedConversionCategory.CZK, 1.0, "CZK") // Czech Republic Koruna
  val DJF = UOM(FixedConversionCategory.DJF, 1.0, "DJF") // Djibouti Franc
  val DKK = UOM(FixedConversionCategory.DKK, 1.0, "DKK") // Denmark Krone
  val DOP = UOM(FixedConversionCategory.DOP, 1.0, "DOP") // Dominican Republic Peso
  val DZD = UOM(FixedConversionCategory.DZD, 1.0, "DZD") // Algeria Dinar
  val EGP = UOM(FixedConversionCategory.EGP, 1.0, "EGP") // Egypt Pound
  val ERN = UOM(FixedConversionCategory.ERN, 1.0, "ERN") // Eritrea Nakfa
  val ETB = UOM(FixedConversionCategory.ETB, 1.0, "ETB") // Ethiopia Birr
  val EUR = UOM(FixedConversionCategory.EUR, 1.0, "EUR") // Euro Member Countries
  val FJD = UOM(FixedConversionCategory.FJD, 1.0, "FJD") // Fiji Dollar
  val FKP = UOM(FixedConversionCategory.FKP, 1.0, "FKP") // Falkland Islands (Malvinas) Pound
  val GBP = UOM(FixedConversionCategory.GBP, 1.0, "GBP") // United Kingdom Pound
  val PENCE = UOM(FixedConversionCategory.GBP, 0.01, "p", "PENCE")
  val GEL = UOM(FixedConversionCategory.GEL, 1.0, "GEL") // Georgia Lari
  val GGP = UOM(FixedConversionCategory.GGP, 1.0, "GGP") // Guernsey Pound
  val GHS = UOM(FixedConversionCategory.GHS, 1.0, "GHS") // Ghana Cedi
  val GIP = UOM(FixedConversionCategory.GIP, 1.0, "GIP") // Gibraltar Pound
  val GMD = UOM(FixedConversionCategory.GMD, 1.0, "GMD") // Gambia Dalasi
  val GNF = UOM(FixedConversionCategory.GNF, 1.0, "GNF") // Guinea Franc
  val GTQ = UOM(FixedConversionCategory.GTQ, 1.0, "GTQ") // Guatemala Quetzal
  val GYD = UOM(FixedConversionCategory.GYD, 1.0, "GYD") // Guyana Dollar
  val HKD = UOM(FixedConversionCategory.HKD, 1.0, "HKD") // Hong Kong Dollar
  val HNL = UOM(FixedConversionCategory.HNL, 1.0, "HNL") // Honduras Lempira
  val HRK = UOM(FixedConversionCategory.HRK, 1.0, "HRK") // Croatia Kuna
  val HTG = UOM(FixedConversionCategory.HTG, 1.0, "HTG") // Haiti Gourde
  val HUF = UOM(FixedConversionCategory.HUF, 1.0, "HUF") // Hungary Forint
  val IDR = UOM(FixedConversionCategory.IDR, 1.0, "IDR") // Indonesia Rupiah
  val ILS = UOM(FixedConversionCategory.ILS, 1.0, "ILS") // Israel Shekel
  val IMP = UOM(FixedConversionCategory.IMP, 1.0, "IMP") // Isle of Man Pound
  val INR = UOM(FixedConversionCategory.INR, 1.0, "INR") // India Rupee
  val IQD = UOM(FixedConversionCategory.IQD, 1.0, "IQD") // Iraq Dinar
  val IRR = UOM(FixedConversionCategory.IRR, 1.0, "IRR") // Iran Rial
  val ISK = UOM(FixedConversionCategory.ISK, 1.0, "ISK") // Iceland Krona
  val JEP = UOM(FixedConversionCategory.JEP, 1.0, "JEP") // Jersey Pound
  val JMD = UOM(FixedConversionCategory.JMD, 1.0, "JMD") // Jamaica Dollar
  val JOD = UOM(FixedConversionCategory.JOD, 1.0, "JOD") // Jordan Dinar
  val JPY = UOM(FixedConversionCategory.JPY, 1.0, "JPY") // Japan Yen
  val KES = UOM(FixedConversionCategory.KES, 1.0, "KES") // Kenya Shilling
  val KGS = UOM(FixedConversionCategory.KGS, 1.0, "KGS") // Kyrgyzstan Som
  val KHR = UOM(FixedConversionCategory.KHR, 1.0, "KHR") // Cambodia Riel
  val KMF = UOM(FixedConversionCategory.KMF, 1.0, "KMF") // Comoros Franc
  val KPW = UOM(FixedConversionCategory.KPW, 1.0, "KPW") // Korea (North) Won
  val KRW = UOM(FixedConversionCategory.KRW, 1.0, "KRW") // Korea (South) Won
  val KWD = UOM(FixedConversionCategory.KWD, 1.0, "KWD") // Kuwait Dinar
  val KYD = UOM(FixedConversionCategory.KYD, 1.0, "KYD") // Cayman Islands Dollar
  val KZT = UOM(FixedConversionCategory.KZT, 1.0, "KZT") // Kazakhstan Tenge
  val LAK = UOM(FixedConversionCategory.LAK, 1.0, "LAK") // Laos Kip
  val LBP = UOM(FixedConversionCategory.LBP, 1.0, "LBP") // Lebanon Pound
  val LKR = UOM(FixedConversionCategory.LKR, 1.0, "LKR") // Sri Lanka Rupee
  val LRD = UOM(FixedConversionCategory.LRD, 1.0, "LRD") // Liberia Dollar
  val LSL = UOM(FixedConversionCategory.LSL, 1.0, "LSL") // Lesotho Loti
  val LTL = UOM(FixedConversionCategory.LTL, 1.0, "LTL") // Lithuania Litas
  val LYD = UOM(FixedConversionCategory.LYD, 1.0, "LYD") // Libya Dinar
  val MAD = UOM(FixedConversionCategory.MAD, 1.0, "MAD") // Morocco Dirham
  val MDL = UOM(FixedConversionCategory.MDL, 1.0, "MDL") // Moldova Leu
  val MGA = UOM(FixedConversionCategory.MGA, 1.0, "MGA") // Madagascar Ariary
  val MKD = UOM(FixedConversionCategory.MKD, 1.0, "MKD") // Macedonia Denar
  val MMK = UOM(FixedConversionCategory.MMK, 1.0, "MMK") // Myanmar (Burma) Kyat
  val MNT = UOM(FixedConversionCategory.MNT, 1.0, "MNT") // Mongolia Tughrik
  val MOP = UOM(FixedConversionCategory.MOP, 1.0, "MOP") // Macau Pataca
  val MRO = UOM(FixedConversionCategory.MRO, 1.0, "MRO") // Mauritania Ouguiya
  val MUR = UOM(FixedConversionCategory.MUR, 1.0, "MUR") // Mauritius Rupee
  val MVR = UOM(FixedConversionCategory.MVR, 1.0, "MVR") // Maldives (Maldive Islands) Rufiyaa
  val MWK = UOM(FixedConversionCategory.MWK, 1.0, "MWK") // Malawi Kwacha
  val MXN = UOM(FixedConversionCategory.MXN, 1.0, "MXN") // Mexico Peso
  val MYR = UOM(FixedConversionCategory.MYR, 1.0, "MYR") // Malaysia Ringgit
  val MZN = UOM(FixedConversionCategory.MZN, 1.0, "MZN") // Mozambique Metical
  val NAD = UOM(FixedConversionCategory.NAD, 1.0, "NAD") // Namibia Dollar
  val NGN = UOM(FixedConversionCategory.NGN, 1.0, "NGN") // Nigeria Naira
  val NIO = UOM(FixedConversionCategory.NIO, 1.0, "NIO") // Nicaragua Cordoba
  val NOK = UOM(FixedConversionCategory.NOK, 1.0, "NOK") // Norway Krone
  val NPR = UOM(FixedConversionCategory.NPR, 1.0, "NPR") // Nepal Rupee
  val NZD = UOM(FixedConversionCategory.NZD, 1.0, "NZD") // New Zealand Dollar
  val OMR = UOM(FixedConversionCategory.OMR, 1.0, "OMR") // Oman Rial
  val PAB = UOM(FixedConversionCategory.PAB, 1.0, "PAB") // Panama Balboa
  val PEN = UOM(FixedConversionCategory.PEN, 1.0, "PEN") // Peru Nuevo Sol
  val PGK = UOM(FixedConversionCategory.PGK, 1.0, "PGK") // Papua New Guinea Kina
  val PHP = UOM(FixedConversionCategory.PHP, 1.0, "PHP") // Philippines Peso
  val PKR = UOM(FixedConversionCategory.PKR, 1.0, "PKR") // Pakistan Rupee
  val PLN = UOM(FixedConversionCategory.PLN, 1.0, "PLN") // Poland Zloty
  val PYG = UOM(FixedConversionCategory.PYG, 1.0, "PYG") // Paraguay Guarani
  val QAR = UOM(FixedConversionCategory.QAR, 1.0, "QAR") // Qatar Riyal
  val RON = UOM(FixedConversionCategory.RON, 1.0, "RON") // Romania New Leu
  val RSD = UOM(FixedConversionCategory.RSD, 1.0, "RSD") // Serbia Dinar
  val RUB = UOM(FixedConversionCategory.RUB, 1.0, "RUB") // Russia Ruble
  val RWF = UOM(FixedConversionCategory.RWF, 1.0, "RWF") // Rwanda Franc
  val SAR = UOM(FixedConversionCategory.SAR, 1.0, "SAR") // Saudi Arabia Riyal
  val SBD = UOM(FixedConversionCategory.SBD, 1.0, "SBD") // Solomon Islands Dollar
  val SCR = UOM(FixedConversionCategory.SCR, 1.0, "SCR") // Seychelles Rupee
  val SDG = UOM(FixedConversionCategory.SDG, 1.0, "SDG") // Sudan Pound
  val SEK = UOM(FixedConversionCategory.SEK, 1.0, "SEK") // Sweden Krona
  val SGD = UOM(FixedConversionCategory.SGD, 1.0, "SGD") // Singapore Dollar
  val SHP = UOM(FixedConversionCategory.SHP, 1.0, "SHP") // Saint Helena Pound
  val SLL = UOM(FixedConversionCategory.SLL, 1.0, "SLL") // Sierra Leone Leone
  val SOS = UOM(FixedConversionCategory.SOS, 1.0, "SOS") // Somalia Shilling
  val SPL = UOM(FixedConversionCategory.SPL, 1.0, "SPL") // Seborga Luigino
  val SRD = UOM(FixedConversionCategory.SRD, 1.0, "SRD") // Suriname Dollar
  val STD = UOM(FixedConversionCategory.STD, 1.0, "STD") // São Tomé and Príncipe Dobra
  val SVC = UOM(FixedConversionCategory.SVC, 1.0, "SVC") // El Salvador Colon
  val SYP = UOM(FixedConversionCategory.SYP, 1.0, "SYP") // Syria Pound
  val SZL = UOM(FixedConversionCategory.SZL, 1.0, "SZL") // Swaziland Lilangeni
  val THB = UOM(FixedConversionCategory.THB, 1.0, "THB") // Thailand Baht
  val TJS = UOM(FixedConversionCategory.TJS, 1.0, "TJS") // Tajikistan Somoni
  val TMT = UOM(FixedConversionCategory.TMT, 1.0, "TMT") // Turkmenistan Manat
  val TND = UOM(FixedConversionCategory.TND, 1.0, "TND") // Tunisia Dinar
  val TOP = UOM(FixedConversionCategory.TOP, 1.0, "TOP") // Tonga Pa'anga
  val TRY = UOM(FixedConversionCategory.TRY, 1.0, "TRY") // Turkey Lira
  val TTD = UOM(FixedConversionCategory.TTD, 1.0, "TTD") // Trinidad and Tobago Dollar
  val TVD = UOM(FixedConversionCategory.TVD, 1.0, "TVD") // Tuvalu Dollar
  val TWD = UOM(FixedConversionCategory.TWD, 1.0, "TWD") // Taiwan New Dollar
  val TZS = UOM(FixedConversionCategory.TZS, 1.0, "TZS") // Tanzania Shilling
  val UAH = UOM(FixedConversionCategory.UAH, 1.0, "UAH") // Ukraine Hryvnia
  val UGX = UOM(FixedConversionCategory.UGX, 1.0, "UGX") // Uganda Shilling
  val USD = UOM(FixedConversionCategory.USD, 1.0, "USD") // United States Dollar
  val US_CENT = UOM(FixedConversionCategory.USD, 0.01, "¢", "USC", "US_CENT")
  val K_USD = UOM(FixedConversionCategory.USD, 1000.0, "K USD")
  val UYU = UOM(FixedConversionCategory.UYU, 1.0, "UYU") // Uruguay Peso
  val UZS = UOM(FixedConversionCategory.UZS, 1.0, "UZS") // Uzbekistan Som
  val VEF = UOM(FixedConversionCategory.VEF, 1.0, "VEF") // Venezuela Bolivar
  val VND = UOM(FixedConversionCategory.VND, 1.0, "VND") // Viet Nam Dong
  val VUV = UOM(FixedConversionCategory.VUV, 1.0, "VUV") // Vanuatu Vatu
  val WST = UOM(FixedConversionCategory.WST, 1.0, "WST") // Samoa Tala
  val XAF = UOM(FixedConversionCategory.XAF, 1.0, "XAF") // Communauté Financière Africaine (1BEAC) CFA Franc BEAC
  val XCD = UOM(FixedConversionCategory.XCD, 1.0, "XCD") // East Caribbean Dollar
  val XDR = UOM(FixedConversionCategory.XDR, 1.0, "XDR") // International Monetary Fund (1IMF) Special Drawing Rights
  val XOF = UOM(FixedConversionCategory.XOF, 1.0, "XOF") // Communauté Financière Africaine (1BCEAO) Franc
  val XPF = UOM(FixedConversionCategory.XPF, 1.0, "XPF") // Comptoirs Français du Pacifique (1CFP) Franc
  val YER = UOM(FixedConversionCategory.YER, 1.0, "YER") // Yemen Rial
  val ZAR = UOM(FixedConversionCategory.ZAR, 1.0, "ZAR") // South Africa Rand
  val ZMW = UOM(FixedConversionCategory.ZMW, 1.0, "ZMW") // Zambia Kwacha
  val ZWD = UOM(FixedConversionCategory.ZWD, 1.0, "ZWD") // Zimbabwe Dollar
  
  // crypto currencies
  val BTC = UOM(FixedConversionCategory.BTC, 1.0, "BTC")
  val ETH = UOM(FixedConversionCategory.ETH, 1.0, "ETH")
  val BCH = UOM(FixedConversionCategory.BCH, 1.0, "BCH")
  val XRP = UOM(FixedConversionCategory.XRP, 1.0, "XRP")
  val LTC = UOM(FixedConversionCategory.LTC, 1.0, "LTC")

  // Forward FX points
  val PIPS = UOM(FixedConversionCategory.PIPS, 1.0, "PIPS")

  // oil
  val BBL = UOM(FixedConversionCategory.ImperialOilVolume, 1.0, "bbl")
  val K_BBL = UOM(FixedConversionCategory.ImperialOilVolume, 1000, "kbbl", "KB")
  val GAL = UOM(FixedConversionCategory.ImperialOilVolume, 1 / BigDecimal(42.0), "gal")
  val K_GAL = UOM(FixedConversionCategory.ImperialOilVolume, 1000 / BigDecimal(42.0), "K gal")
  val L = UOM(FixedConversionCategory.MetricOilVolume, 1.0, "l")
  val KL = UOM(FixedConversionCategory.MetricOilVolume, 1000.0, "kl")
  val M3 = UOM(FixedConversionCategory.MetricOilVolume, 1000.0, "m3")
  val CM3 = UOM(FixedConversionCategory.MetricOilVolume, 1e-3, "cm3")

  // gas
  val THM = UOM(FixedConversionCategory.GasEnergy, 1.0, "THM")
  val K_THM = UOM(FixedConversionCategory.GasEnergy, 1000.0, "K THM")
  val MMBTU = UOM(FixedConversionCategory.GasEnergy, 10.0, "MMBTU")

   // mass
  val G = UOM(FixedConversionCategory.Mass, 1.0, "g")
  val DAG = UOM(FixedConversionCategory.Mass, 10.0, "dag")
  val KG = UOM(FixedConversionCategory.Mass, 1000.0, "kg")
  val MT = UOM(FixedConversionCategory.Mass, 1e6, "MT")
  val H_MT = UOM(FixedConversionCategory.Mass, 1e8, "HMT")
  val KT = UOM(FixedConversionCategory.Mass, 1e9, "KT")
  val LB = UOM(FixedConversionCategory.Mass, BigDecimal("453.59237"), "lb")
  val TROY_OZ = UOM(FixedConversionCategory.Mass, BigDecimal("31.1034768"), "ozt")

  // power
  val WATT = UOM(FixedConversionCategory.Power, 1.0, "W")
  val KW = UOM(FixedConversionCategory.Power, 1000.0, "KW")
  val MW = UOM(FixedConversionCategory.Power, 1000.0 * 1000.0, "MW")


  // time
  val SECOND = UOM(FixedConversionCategory.Time, 1.0, "sec")
  val HOUR = UOM(FixedConversionCategory.Time, 60 * 60, "Hour")
  val DAY = UOM(FixedConversionCategory.Time, 24 * 60 * 60, "Day")

  // energy
  val MWH: UOM = MW * HOUR

  // length
  val CM = UOM(FixedConversionCategory.Length, 1, "cm")
  val M = UOM(FixedConversionCategory.Length, 100, "m")

  // Lots
  val LOTS = UOM(FixedConversionCategory.Lots, 1, "Lot", "Lots")
  
  // Equity
  val SHARE = UOM(FixedConversionCategory.Equity, 1, "share")

  private lazy val byName = scala.collection.mutable.Map[String, UOM]()
  byName += ("null" -> NULL)
  byName += ("scalar" -> SCALAR)
  byName += ("mwh" -> MWH)
  def fromName(name: String): Option[UOM] = byName.get(name.toLowerCase)

  val allUOMs: Seq[UOM] = byName.values.toSeq.filterNot(Seq(NULL, SCALAR, PIPS).contains)

  val wellKnownGroupings: Map[(String, String), String] = Map(("MW", "Hour") -> "MWH")

  /**
   * @param category Mass, Time, Length etc
   * @param relationToBase Use 1.0 as the base and then relationToBase is a multiple to convert this to the base
   * @param names A list of names. The first one will be the primary name and used in toString
   * @param builder The implementation specific builder
   */
  private def apply(category: FixedConversionCategory, relationToBase: scala.BigDecimal, names: String*)
                   (implicit builder: UOMBuilder): UOM = {
    val uom = builder.build(category, relationToBase, names: _*)
    names.foreach{name => byName += (name.toLowerCase -> uom)}
    uom
  }

  /**
   * A UOM wrapper for a UOM which is just something like BBL or USD^2 and not USDBBL
   */
  trait SingleUOM {
    def power: Int

    def base: SingleUOM
    def conversionToBase: scala.BigDecimal

    def conversionFactor(to: UOM.SingleUOM, conversions: DimensionConversions): Either[TopazFail, BigDecimal]
    def asUOM: UOM
  }
}
