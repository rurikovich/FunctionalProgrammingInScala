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
    check {
      forAll(Gen.asciiStr) {
        str =>
          val s=str.quoted
          s.withoutQuotes() == str
      }
    }
  }

  "framedBy" should "operate correctly" in {
    import org.scalacheck.Arbitrary.arbitrary

    val nonEmptyStrGen = arbitrary[String].suchThat(!_.isEmpty)

    check {
      forAll(nonEmptyStrGen) {
        str =>
          str.quoted.framedBy(quote)
      }
    }

    val leftFrame: String = "["
    val rightFrame: String = "]"
    check {
      forAll(nonEmptyStrGen) {
        str =>
          val s: String = s"""$leftFrame$str$rightFrame""".stripMargin
          s.framedBy(leftFrame, rightFrame)
      }
    }


    "123456".framedBy("1234567") shouldBe false
    "123456".framedBy("123456", "123456") shouldBe false
    "123456".framedBy("123", "456") shouldBe false
    "123456".framedBy("123", "3456") shouldBe false
  }

  "withoutFrame" should "operate correctly" in {
    val gen = Gen.stringOf(Gen.oneOf[Char](List('a', 'b', 'c')))
    check {
      forAll(gen) {
        str =>
          str.withoutFrame("d").isEmpty
      }
    }

    val n = 10
    val genStr = Gen.stringOfN(n, Gen.asciiChar)
    val genLongFrame = Gen.stringOfN(n / 2 + 1, Gen.asciiChar)
    check {
      forAll(genStr, genLongFrame) {
        case (str, frame) =>
          str.withoutFrame(frame).isEmpty
      }
    }

    val aCharGen = Gen.oneOf[Char](List('a'))
    val genConstStr = Gen.stringOfN(n, aCharGen)
    val genShortFrame = Gen.stringOfN(n / 2 - 1, aCharGen)
    check {
      forAll(genConstStr, genShortFrame) {
        case (str, frame) =>
          str.withoutFrame(frame).isDefined
      }
    }

  }

}
