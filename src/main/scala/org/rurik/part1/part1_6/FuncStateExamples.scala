package org.rurik.part1.part1_6

import scala.::
import scala.Int._

object FuncStateExamples {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = rng.nextInt match {
    case (v, newRng) =>
      (v / (MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (d1, r1) = nonNegativeInt(rng)
    val (d2, r2) = double(r1)
    ((d1, d2), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = nonNegativeInt(r1)
    ((d1, d2), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = (0 until count).foldLeft((List.empty[Int], rng)) {
    case ((list: List[Int], r: RNG), _) =>
      val (v: Int, newRng: RNG) = nonNegativeInt(r)
      (list ++ List(v), newRng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleMap(rng: RNG): (Double, RNG) = map[Int, Double](_.nextInt) {
    _ / (MaxValue.toDouble + 1)
  }(rng)

  def unit[A](a: A): Rand[A] = rng => (a, rng.nextInt._2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng: RNG => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val int: Rand[Int] = _.nextInt

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft((List.empty[A], _: RNG)) {
    (rndList, rnd) =>
      map2(rndList, rnd)((list: List[A], a: A) => list ++ List(a))
  }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence[Int](List.fill(count)(_.nextInt))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    val randA: Rand[Int] = nonNegativeInt

    val randBFn: Int => Rand[Int] =
      (i: Int) => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    flatMap(randA)(randBFn)
  }

  def mapFM[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2FM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) {
    a: A => {
      rnd => {
        val (b, rB) = rb(rnd)
        (f(a, b), rB)
      }
    }

  }

}
