package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.json.StrHelper
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class StrHelperSpec extends AnyFlatSpec with Checkers with should.Matchers {

  import StrHelper._

  "strWithoutQuotes" should "operate correctly" in {
    val strGen = Gen.asciiStr

    check {
      forAll(strGen) {
        case (str) =>
          val s: String = s""""$str"""".stripMargin
          s.withoutQuotes() == str
      }
    }

  }

}
