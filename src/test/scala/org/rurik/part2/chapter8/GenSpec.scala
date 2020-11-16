package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.SimpleRNG
import org.rurik.part2.chapter8.Gen.choose
import org.scalacheck
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class GenSpec extends AnyFlatSpec with Checkers with should.Matchers {

  "choose" should "gen new values between start and end correctly" in {
    val rnd = SimpleRNG(1)

    val startGen = org.scalacheck.Gen.choose(0, 100)
    val endGen = org.scalacheck.Gen.choose(101, 200)

    check {
      forAll(startGen, endGen) {
        case (start, end) =>
          val v = choose(start, end).sample.run(rnd)._1
          v > start && v < end
      }
    }


  }

}
