package org.rurik.part3.chapter10

import org.rurik.part3.chapter10.Monoid.Laws._
import org.rurik.part3.chapter10.Monoids._
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class MonoidLawsSpec extends AnyFlatSpec with Checkers with should.Matchers {

  "Monoids" should "satisfy laws" in {

    val intGen = Gen.chooseNum(Int.MinValue, Int.MaxValue)
    val boolGen = Gen.oneOf(true, false)
    val optionGen: Gen[Option[Int]] = intGen.map((i: Int) => if (i > 0) Some(i) else None)
    val posIntGen = Gen.chooseNum(0, 1000)

    check(monoidLaws(intAddition, intGen))
    check(monoidLaws(intMultiplication, intGen))
    check(monoidLaws(booleanAnd, boolGen))
    check(monoidLaws(optionMonoid[Int], optionGen))

    check(monoidLawsFn[Int](endoMonoid[Int], posIntGen, a => (i: Int) => i + a))

  }

}
