package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.helpers.StrHelper
import org.rurik.part2.chapter9.v2.RegexPatterns._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class RegexSpec extends AnyFlatSpec with Checkers with should.Matchers {

  "numberPattern" should "parse numbers to double correctly" in {

    parseDouble("0.111" + "some end of input str") shouldEqual Some(0.111)

    parseDouble("123" + "some end of input str") shouldEqual Some(123)

    parseDouble("345.456" + "some end of input str") shouldEqual Some(345.456)

  }

  "stringPattern" should "parse string with quotes correctly" in {
    import StrHelper._

    parseQuotedStr("aaa".quoted + "some end of input str") shouldEqual Some("aaa".quoted)

    parseQuotedStr("".quoted ) shouldEqual Some("".quoted)

  }


  def parseDouble(regex: String): Option[Double] = {
    numberPattern.findPrefixMatchOf(regex).map(_.toString.toDouble)
  }


  def parseQuotedStr(regex: String): Option[String] = {
    stringPattern.findPrefixMatchOf(regex).map(_.toString)
  }


}
