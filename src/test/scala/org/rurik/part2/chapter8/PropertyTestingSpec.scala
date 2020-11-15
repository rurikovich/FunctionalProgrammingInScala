package org.rurik.part2.chapter8

import org.rurik.part2.chapter8.PropertyTesting.{max, sum}
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, _}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.Checkers

class PropertyTestingSpec extends AnyFlatSpec with Checkers {

  "sum" should "hold on laws correctly" in {
    val intList = Gen.listOf(Gen.choose(0, 100))

    val constValue = 1
    val constList = Gen.listOf(Gen.const(constValue))

    check {
      forAll(intList)(ns => sum(ns) == sum(ns.reverse))
    }

    check {
      forAll(constList)(ns => sum(ns) == ns.size * constValue)
    }
  }


  "max" should "hold on laws correctly" in {
    val constValue = 1
    val constList = Gen.nonEmptyListOf(Gen.const(constValue))
    check {
      forAll(constList)(ns => max(ns) == constValue)
    }
  }


}
