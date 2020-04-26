package com.topaz.quantity

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PrimesTest extends AnyFunSuite with Matchers {

  val primes = 2 #:: sieve(3)

  def sieve(n: Int): Stream[Int] =
    if (primes.takeWhile(p => p * p <= n).exists(n % _ == 0)) sieve(n + 2)
    else n #:: sieve(n + 2)

  test("Primes.primes should generate primes") {
    // sanity check taken from http://en.wikipedia.org/wiki/List_of_prime_numbers
    Primes.primes(1).take(20).toList shouldEqual
      List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)

    // test against different generator
    Primes.primes(1).take(5000).toList shouldEqual primes.take(5000).toList
  }

  test("starting prime should be greater than or equal to input") {
    Primes.primes(17).take(2).toList shouldEqual List(17, 19)
    Primes.primes(18).take(1).toList shouldEqual List(19)
    Primes.primes(0).take(1).toList shouldEqual List(2)
  }

  test("test factoring") {
    val first10 = Primes.primes().take(10).toList
    val first50 = Primes.primes().take(50).toList

    Primes.factor(2 * 2, first50) shouldEqual List(2, 2)
    Primes.factor(3 * 11 * 67 * 67, first50).toSet shouldEqual Set(3, 11, 67, 67)

    Primes.factor(first10.foldLeft(1l)(_ * _), first50).toSet shouldEqual first10.toSet
  }

  test("can fail to factor if not given the right primes") {
    Primes.factor(25, List(2, 3)) shouldEqual Nil
  }

  test("2^6 × 3^5 × 5^6 = 243,000,000") {
    Primes.factor(243000000l, primes.take(5).toList) shouldEqual
      List(5, 5, 5, 5, 5, 5, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2)
  }

  test("1 factors into an empty list") {
    Primes.factor(1, Nil) should be ('empty)
  }
}
