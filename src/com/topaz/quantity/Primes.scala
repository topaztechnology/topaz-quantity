package com.topaz.quantity

import com.topaz.TopazCodingError
import org.apache.commons.math3.primes.{Primes => APrimes}

import scala.annotation.tailrec

object Primes {
  def primes(from: Int = 2): Iterator[Int] = {
    Iterator.iterate(APrimes.nextPrime(from))(prev => APrimes.nextPrime(prev + 1))
  }

  def factor(number: Long, possiblePrimes: Seq[Int]) = {
    if (number == 1)
      Nil
    else {
      require(number > 1, "Invalid value: " + number)
      @tailrec
      def find(n: Long, factors: List[Int]): List[Int] = if (n == 1) {
        factors
      } else {
        possiblePrimes.find(prime => n % prime == 0) match {
          case Some(factor) => find(n / factor, factor :: factors)
          case _ => Nil
        }
      }
      find(number, Nil)
    }
  }
}

/**
 * Primes.factors (above) needs a list of possiblePrimes. If you use this class instead the max prime is pre-defined
 * so we know the list of possiblePrimes in advance.
 */
object LimitedPrimeGenerator {
  val MaxPrime = 3571

  val primesCache: List[Int] = Primes.primes().takeWhile(_ <= MaxPrime).toList.sorted

  def primes: Iterator[Int] = Primes.primes().map{
    case prime if prime > MaxPrime =>
      throw new TopazCodingError(s"Tried to use a prime bigger than $MaxPrime, possibly need to extend?")
    case p => p
  }

  def factor(number: Long) = {
    Primes.factor(number, primesCache)
  }

}
