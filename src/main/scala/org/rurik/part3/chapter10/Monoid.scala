package org.rurik.part3.chapter10

import org.rurik.part3.chapter10.Monoids.endoMonoid
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  object Laws {

    def identityLaw[A](m: Monoid[A], gen: Gen[A]): Prop =
      forAll(gen)(a => m.op(m.zero, a) == a)

    def associativeLaw[A](m: Monoid[A], gen: Gen[A]): Prop =
      forAll(gen, gen, gen) {
        case (x, y, z) =>
          m.op(m.op(x, y), z) == m.op(x, m.op(y, z))
      }

    def associativeLawFn[A](m: Monoid[A => A], x: A => A, y: A => A, z: A => A, valueGen: Gen[A]): Prop = {
      val leftFn = m.op(m.op(x, y), z)
      val rightFn = m.op(x, m.op(y, z))
      forAll(valueGen) {
        i => leftFn(i) == rightFn(i)
      }
    }

    def identityLawFn[A](m: Monoid[A => A], f: A => A, valueGen: Gen[A]): Prop = {
      forAll(valueGen) {
        i => m.op(m.zero, f)(i) == f(i)
      }
    }

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
      identityLaw(m, gen) &&
        associativeLaw(m, gen)


    def monoidLawsFn[A](m: Monoid[A => A], gen: Gen[A], fnMap: A => A => A): Prop = {
      val fnGen = gen.map(fnMap)
      forAll(fnGen, fnGen, fnGen) {
        case (xFn, yFn, zFn) =>
          associativeLawFn[A](m, xFn, yFn, zFn, gen) &&
            identityLawFn[A](m, xFn, gen)
      }
    }

  }


}
