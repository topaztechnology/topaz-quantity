package com.topaz.quantity

sealed case class FixedConversionCategory(prime: Int, name: String)

object FixedConversionCategory {
  private val primes = LimitedPrimeGenerator.primes

  val SCALAR = FixedConversionCategory(1, "SCALAR")

  val Lots = FixedConversionCategory(primes.next(), "Lots")

  val Power = FixedConversionCategory(primes.next(),  "Power")
  val Energy = FixedConversionCategory(primes.next(), "Energy")
  val Mass = FixedConversionCategory(primes.next(),   "Mass")
  val Time = FixedConversionCategory(primes.next(),   "Time")
  val Length = FixedConversionCategory(primes.next(), "Length")

  // An oil barrel (abbreviated as bbl) is a unit of volume whose definition has not been universally standardized.
  // from http://en.wikipedia.org/wiki/Oil_barrel#Oil_barrel
  val ImperialOilVolume = FixedConversionCategory(primes.next(), "ImperialOilVolume")
  val MetricOilVolume = FixedConversionCategory(primes.next(), "MetricOilVolume")

  // One British therm (symbol thm) is a non-SI unit of heat energy equal to 100,000 British thermal units (BTU).
  // It is approximately the energy equivalent of burning 100 cubic feet (often referred to as 1 CCF) of natural gas.
  // from http://en.wikipedia.org/wiki/Therm
  val GasEnergy = FixedConversionCategory(primes.next(), "GasVolume")
  
  // Equity
  val Equity = FixedConversionCategory(primes.next(), "Equity")

  // currencies
  val AED = FixedConversionCategory(primes.next(), "AED") // United Arab Emirates Dirham
  val AFN = FixedConversionCategory(primes.next(), "AFN") // Afghanistan Afghani
  val ALL = FixedConversionCategory(primes.next(), "ALL") // Albania Lek
  val AMD = FixedConversionCategory(primes.next(), "AMD") // Armenia Dram
  val ANG = FixedConversionCategory(primes.next(), "ANG") // Netherlands Antilles Guilder
  val AOA = FixedConversionCategory(primes.next(), "AOA") // Angola Kwanza
  val ARS = FixedConversionCategory(primes.next(), "ARS") // Argentina Peso
  val AUD = FixedConversionCategory(primes.next(), "AUD") // Australia Dollar
  val AWG = FixedConversionCategory(primes.next(), "AWG") // Aruba Guilder
  val AZN = FixedConversionCategory(primes.next(), "AZN") // Azerbaijan New Manat
  val BAM = FixedConversionCategory(primes.next(), "BAM") // Bosnia and Herzegovina Convertible Marka
  val BBD = FixedConversionCategory(primes.next(), "BBD") // Barbados Dollar
  val BDT = FixedConversionCategory(primes.next(), "BDT") // Bangladesh Taka
  val BGN = FixedConversionCategory(primes.next(), "BGN") // Bulgaria Lev
  val BHD = FixedConversionCategory(primes.next(), "BHD") // Bahrain Dinar
  val BIF = FixedConversionCategory(primes.next(), "BIF") // Burundi Franc
  val BMD = FixedConversionCategory(primes.next(), "BMD") // Bermuda Dollar
  val BND = FixedConversionCategory(primes.next(), "BND") // Brunei Darussalam Dollar
  val BOB = FixedConversionCategory(primes.next(), "BOB") // Bolivia Boliviano
  val BRL = FixedConversionCategory(primes.next(), "BRL") // Brazil Real
  val BSD = FixedConversionCategory(primes.next(), "BSD") // Bahamas Dollar
  val BTN = FixedConversionCategory(primes.next(), "BTN") // Bhutan Ngultrum
  val BWP = FixedConversionCategory(primes.next(), "BWP") // Botswana Pula
  val BYR = FixedConversionCategory(primes.next(), "BYR") // Belarus Ruble
  val BZD = FixedConversionCategory(primes.next(), "BZD") // Belize Dollar
  val CAD = FixedConversionCategory(primes.next(), "CAD") // Canada Dollar
  val CDF = FixedConversionCategory(primes.next(), "CDF") // Congo/Kinshasa Franc
  val CHF = FixedConversionCategory(primes.next(), "CHF") // Switzerland Franc
  val CLP = FixedConversionCategory(primes.next(), "CLP") // Chile Peso
  val CNY = FixedConversionCategory(primes.next(), "CNY") // China Yuan Renminbi
  val COP = FixedConversionCategory(primes.next(), "COP") // Colombia Peso
  val CRC = FixedConversionCategory(primes.next(), "CRC") // Costa Rica Colon
  val CUC = FixedConversionCategory(primes.next(), "CUC") // Cuba Convertible Peso
  val CUP = FixedConversionCategory(primes.next(), "CUP") // Cuba Peso
  val CVE = FixedConversionCategory(primes.next(), "CVE") // Cape Verde Escudo
  val CZK = FixedConversionCategory(primes.next(), "CZK") // Czech Republic Koruna
  val DJF = FixedConversionCategory(primes.next(), "DJF") // Djibouti Franc
  val DKK = FixedConversionCategory(primes.next(), "DKK") // Denmark Krone
  val DOP = FixedConversionCategory(primes.next(), "DOP") // Dominican Republic Peso
  val DZD = FixedConversionCategory(primes.next(), "DZD") // Algeria Dinar
  val EGP = FixedConversionCategory(primes.next(), "EGP") // Egypt Pound
  val ERN = FixedConversionCategory(primes.next(), "ERN") // Eritrea Nakfa
  val ETB = FixedConversionCategory(primes.next(), "ETB") // Ethiopia Birr
  val EUR = FixedConversionCategory(primes.next(), "EUR") // Euro Member Countries
  val FJD = FixedConversionCategory(primes.next(), "FJD") // Fiji Dollar
  val FKP = FixedConversionCategory(primes.next(), "FKP") // Falkland Islands (Malvinas) Pound
  val GBP = FixedConversionCategory(primes.next(), "GBP") // United Kingdom Pound
  val GEL = FixedConversionCategory(primes.next(), "GEL") // Georgia Lari
  val GGP = FixedConversionCategory(primes.next(), "GGP") // Guernsey Pound
  val GHS = FixedConversionCategory(primes.next(), "GHS") // Ghana Cedi
  val GIP = FixedConversionCategory(primes.next(), "GIP") // Gibraltar Pound
  val GMD = FixedConversionCategory(primes.next(), "GMD") // Gambia Dalasi
  val GNF = FixedConversionCategory(primes.next(), "GNF") // Guinea Franc
  val GTQ = FixedConversionCategory(primes.next(), "GTQ") // Guatemala Quetzal
  val GYD = FixedConversionCategory(primes.next(), "GYD") // Guyana Dollar
  val HKD = FixedConversionCategory(primes.next(), "HKD") // Hong Kong Dollar
  val HNL = FixedConversionCategory(primes.next(), "HNL") // Honduras Lempira
  val HRK = FixedConversionCategory(primes.next(), "HRK") // Croatia Kuna
  val HTG = FixedConversionCategory(primes.next(), "HTG") // Haiti Gourde
  val HUF = FixedConversionCategory(primes.next(), "HUF") // Hungary Forint
  val IDR = FixedConversionCategory(primes.next(), "IDR") // Indonesia Rupiah
  val ILS = FixedConversionCategory(primes.next(), "ILS") // Israel Shekel
  val IMP = FixedConversionCategory(primes.next(), "IMP") // Isle of Man Pound
  val INR = FixedConversionCategory(primes.next(), "INR") // India Rupee
  val IQD = FixedConversionCategory(primes.next(), "IQD") // Iraq Dinar
  val IRR = FixedConversionCategory(primes.next(), "IRR") // Iran Rial
  val ISK = FixedConversionCategory(primes.next(), "ISK") // Iceland Krona
  val JEP = FixedConversionCategory(primes.next(), "JEP") // Jersey Pound
  val JMD = FixedConversionCategory(primes.next(), "JMD") // Jamaica Dollar
  val JOD = FixedConversionCategory(primes.next(), "JOD") // Jordan Dinar
  val JPY = FixedConversionCategory(primes.next(), "JPY") // Japan Yen
  val KES = FixedConversionCategory(primes.next(), "KES") // Kenya Shilling
  val KGS = FixedConversionCategory(primes.next(), "KGS") // Kyrgyzstan Som
  val KHR = FixedConversionCategory(primes.next(), "KHR") // Cambodia Riel
  val KMF = FixedConversionCategory(primes.next(), "KMF") // Comoros Franc
  val KPW = FixedConversionCategory(primes.next(), "KPW") // Korea (North) Won
  val KRW = FixedConversionCategory(primes.next(), "KRW") // Korea (South) Won
  val KWD = FixedConversionCategory(primes.next(), "KWD") // Kuwait Dinar
  val KYD = FixedConversionCategory(primes.next(), "KYD") // Cayman Islands Dollar
  val KZT = FixedConversionCategory(primes.next(), "KZT") // Kazakhstan Tenge
  val LAK = FixedConversionCategory(primes.next(), "LAK") // Laos Kip
  val LBP = FixedConversionCategory(primes.next(), "LBP") // Lebanon Pound
  val LKR = FixedConversionCategory(primes.next(), "LKR") // Sri Lanka Rupee
  val LRD = FixedConversionCategory(primes.next(), "LRD") // Liberia Dollar
  val LSL = FixedConversionCategory(primes.next(), "LSL") // Lesotho Loti
  val LTL = FixedConversionCategory(primes.next(), "LTL") // Lithuania Litas
  val LYD = FixedConversionCategory(primes.next(), "LYD") // Libya Dinar
  val MAD = FixedConversionCategory(primes.next(), "MAD") // Morocco Dirham
  val MDL = FixedConversionCategory(primes.next(), "MDL") // Moldova Leu
  val MGA = FixedConversionCategory(primes.next(), "MGA") // Madagascar Ariary
  val MKD = FixedConversionCategory(primes.next(), "MKD") // Macedonia Denar
  val MMK = FixedConversionCategory(primes.next(), "MMK") // Myanmar (Burma) Kyat
  val MNT = FixedConversionCategory(primes.next(), "MNT") // Mongolia Tughrik
  val MOP = FixedConversionCategory(primes.next(), "MOP") // Macau Pataca
  val MRO = FixedConversionCategory(primes.next(), "MRO") // Mauritania Ouguiya
  val MUR = FixedConversionCategory(primes.next(), "MUR") // Mauritius Rupee
  val MVR = FixedConversionCategory(primes.next(), "MVR") // Maldives (Maldive Islands) Rufiyaa
  val MWK = FixedConversionCategory(primes.next(), "MWK") // Malawi Kwacha
  val MXN = FixedConversionCategory(primes.next(), "MXN") // Mexico Peso
  val MYR = FixedConversionCategory(primes.next(), "MYR") // Malaysia Ringgit
  val MZN = FixedConversionCategory(primes.next(), "MZN") // Mozambique Metical
  val NAD = FixedConversionCategory(primes.next(), "NAD") // Namibia Dollar
  val NGN = FixedConversionCategory(primes.next(), "NGN") // Nigeria Naira
  val NIO = FixedConversionCategory(primes.next(), "NIO") // Nicaragua Cordoba
  val NOK = FixedConversionCategory(primes.next(), "NOK") // Norway Krone
  val NPR = FixedConversionCategory(primes.next(), "NPR") // Nepal Rupee
  val NZD = FixedConversionCategory(primes.next(), "NZD") // New Zealand Dollar
  val OMR = FixedConversionCategory(primes.next(), "OMR") // Oman Rial
  val PAB = FixedConversionCategory(primes.next(), "PAB") // Panama Balboa
  val PEN = FixedConversionCategory(primes.next(), "PEN") // Peru Nuevo Sol
  val PGK = FixedConversionCategory(primes.next(), "PGK") // Papua New Guinea Kina
  val PHP = FixedConversionCategory(primes.next(), "PHP") // Philippines Peso
  val PKR = FixedConversionCategory(primes.next(), "PKR") // Pakistan Rupee
  val PLN = FixedConversionCategory(primes.next(), "PLN") // Poland Zloty
  val PYG = FixedConversionCategory(primes.next(), "PYG") // Paraguay Guarani
  val QAR = FixedConversionCategory(primes.next(), "QAR") // Qatar Riyal
  val RON = FixedConversionCategory(primes.next(), "RON") // Romania New Leu
  val RSD = FixedConversionCategory(primes.next(), "RSD") // Serbia Dinar
  val RUB = FixedConversionCategory(primes.next(), "RUB") // Russia Ruble
  val RWF = FixedConversionCategory(primes.next(), "RWF") // Rwanda Franc
  val SAR = FixedConversionCategory(primes.next(), "SAR") // Saudi Arabia Riyal
  val SBD = FixedConversionCategory(primes.next(), "SBD") // Solomon Islands Dollar
  val SCR = FixedConversionCategory(primes.next(), "SCR") // Seychelles Rupee
  val SDG = FixedConversionCategory(primes.next(), "SDG") // Sudan Pound
  val SEK = FixedConversionCategory(primes.next(), "SEK") // Sweden Krona
  val SGD = FixedConversionCategory(primes.next(), "SGD") // Singapore Dollar
  val SHP = FixedConversionCategory(primes.next(), "SHP") // Saint Helena Pound
  val SLL = FixedConversionCategory(primes.next(), "SLL") // Sierra Leone Leone
  val SOS = FixedConversionCategory(primes.next(), "SOS") // Somalia Shilling
  val SPL = FixedConversionCategory(primes.next(), "SPL") // Seborga Luigino
  val SRD = FixedConversionCategory(primes.next(), "SRD") // Suriname Dollar
  val STD = FixedConversionCategory(primes.next(), "STD") // São Tomé and Príncipe Dobra
  val SVC = FixedConversionCategory(primes.next(), "SVC") // El Salvador Colon
  val SYP = FixedConversionCategory(primes.next(), "SYP") // Syria Pound
  val SZL = FixedConversionCategory(primes.next(), "SZL") // Swaziland Lilangeni
  val THB = FixedConversionCategory(primes.next(), "THB") // Thailand Baht
  val TJS = FixedConversionCategory(primes.next(), "TJS") // Tajikistan Somoni
  val TMT = FixedConversionCategory(primes.next(), "TMT") // Turkmenistan Manat
  val TND = FixedConversionCategory(primes.next(), "TND") // Tunisia Dinar
  val TOP = FixedConversionCategory(primes.next(), "TOP") // Tonga Pa'anga
  val TRY = FixedConversionCategory(primes.next(), "TRY") // Turkey Lira
  val TTD = FixedConversionCategory(primes.next(), "TTD") // Trinidad and Tobago Dollar
  val TVD = FixedConversionCategory(primes.next(), "TVD") // Tuvalu Dollar
  val TWD = FixedConversionCategory(primes.next(), "TWD") // Taiwan New Dollar
  val TZS = FixedConversionCategory(primes.next(), "TZS") // Tanzania Shilling
  val UAH = FixedConversionCategory(primes.next(), "UAH") // Ukraine Hryvnia
  val UGX = FixedConversionCategory(primes.next(), "UGX") // Uganda Shilling
  val USD = FixedConversionCategory(primes.next(), "USD") // United States Dollar
  val UYU = FixedConversionCategory(primes.next(), "UYU") // Uruguay Peso
  val UZS = FixedConversionCategory(primes.next(), "UZS") // Uzbekistan Som
  val VEF = FixedConversionCategory(primes.next(), "VEF") // Venezuela Bolivar
  val VND = FixedConversionCategory(primes.next(), "VND") // Viet Nam Dong
  val VUV = FixedConversionCategory(primes.next(), "VUV") // Vanuatu Vatu
  val WST = FixedConversionCategory(primes.next(), "WST") // Samoa Tala
  val XAF = FixedConversionCategory(primes.next(), "XAF") // Communauté Financière Africaine (1BEAC) CFA Franc BEAC
  val XCD = FixedConversionCategory(primes.next(), "XCD") // East Caribbean Dollar
  val XDR = FixedConversionCategory(primes.next(), "XDR") // International Monetary Fund (1IMF) Special Drawing Rights
  val XOF = FixedConversionCategory(primes.next(), "XOF") // Communauté Financière Africaine (1BCEAO) Franc
  val XPF = FixedConversionCategory(primes.next(), "XPF") // Comptoirs Français du Pacifique (1CFP) Franc
  val YER = FixedConversionCategory(primes.next(), "YER") // Yemen Rial
  val ZAR = FixedConversionCategory(primes.next(), "ZAR") // South Africa Rand
  val ZMW = FixedConversionCategory(primes.next(), "ZMW") // Zambia Kwacha
  val ZWD = FixedConversionCategory(primes.next(), "ZWD") // Zimbabwe Dollar
  
  // crypto currencies
  val BTC = FixedConversionCategory(primes.next(), "BTC") // Bitcoin
  val ETH = FixedConversionCategory(primes.next(), "ETH") // Ethereum
  val BCH = FixedConversionCategory(primes.next(), "BCH") // Bitcoin Cash
  val XRP = FixedConversionCategory(primes.next(), "XRP") // Ripple
  val LTC = FixedConversionCategory(primes.next(), "LTC") // Litecoin

  val currencyPrimes: Set[Int] =
    Set(AED,AFN,ALL,AMD,ANG,AOA,ARS,AUD,AWG,AZN,BAM,BBD,BDT,BGN,BHD,BIF,BMD,BND,BOB,BRL,BSD,BTN,BWP,BYR,
    BZD,CAD,CDF,CHF,CLP,CNY,COP,CRC,CUC,CUP,CVE,CZK,DJF,DKK,DOP,DZD,EGP,ERN,ETB,EUR,FJD,FKP,GBP,GEL,GGP,GHS,GIP,GMD,
    GNF,GTQ,GYD,HKD,HNL,HRK,HTG,HUF,IDR,ILS,IMP,INR,IQD,IRR,ISK,JEP,JMD,JOD,JPY,KES,KGS,KHR,KMF,KPW,KRW,KWD,KYD,KZT,
    LAK,LBP,LKR,LRD,LSL,LTL,LYD,MAD,MDL,MGA,MKD,MMK,MNT,MOP,MRO,MUR,MVR,MWK,MXN,MYR,MZN,NAD,NGN,NIO,NOK,NPR,NZD,OMR,
    PAB,PEN,PGK,PHP,PKR,PLN,PYG,QAR,RON,RSD,RUB,RWF,SAR,SBD,SCR,SDG,SEK,SGD,SHP,SLL,SOS,SPL,SRD,STD,SVC,SYP,SZL,THB,
    TJS,TMT,TND,TOP,TRY,TTD,TVD,TWD,TZS,UAH,UGX,USD,UYU,UZS,VEF,VND,VUV,WST,XAF,XCD,XDR,XOF,XPF,YER,ZAR,ZMW,ZWD,
    BTC, ETH, BCH, XRP, LTC
    ).map{
      _.prime
    }

  // FX pips - 1/10000th of a unit fx rate. Used in consructing FX forward curves
  val PIPS: FixedConversionCategory = FixedConversionCategory(primes.next(), "PIPS")
}
