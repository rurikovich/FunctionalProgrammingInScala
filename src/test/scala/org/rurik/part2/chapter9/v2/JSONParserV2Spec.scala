package org.rurik.part2.chapter9.v2

import org.rurik.part2.chapter9.json.JSON
import org.rurik.part2.chapter9.json.JSON._
import org.rurik.part2.chapter9.v2.JsonParsersV2.JsonParser
import org.rurik.part2.chapter9.v2.parsers.{Location, Success}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers
import org.rurik.part2.chapter9.helpers.StrHelper._


class JSONParserV2Spec extends AnyFlatSpec with Checkers with should.Matchers {

  val parsers: JsonParsersV2 = new JsonParsersV2()

  import parsers._

  "succeed" should "parse any str to constant value correctly" in {
    val strGen = Gen.asciiStr
    val jsonGen = Gen.oneOf(JNull, JNumber(1), JString(""), JBool(true))

    check {
      forAll(strGen, jsonGen) {
        case (str, json) =>
          val parser: JsonParser[JSON] = parsers.succeed[JSON](json)
          parser.run(str) == Success(json, 0)
      }
    }
  }

  "JNullParser" should "parse \"null\" to JNull correctly" in {
    val nullStr = "null"
    JNullParser.run(nullStr) shouldEqual Success(JNull, nullStr.length)
  }

  "JNumberParser" should "parse any number to double correctly" in {

    val d1 = 0.45
    JNumberParser.run(s"0.45") shouldEqual Success(JNumber(d1), d1.toString.length)

    val numGen = Gen.double
    check {
      forAll(numGen) {
        d =>
          JNumberParser.run(s"$d") == Success(JNumber(d), d.toString.length)
      }
    }
  }

  "JStringParser" should "parse any quoted string to string correctly" in {

    val str1=""
    JStringParser.run(str1.quoted) shouldEqual Success(JString(str1), str1.quoted.length)


//    val strGen = Gen.stringOfN(10, Gen.alphaLowerChar)
//    check {
//      forAll(strGen) {
//        str =>
//          JStringParser.run(str.quoted) == Success(JString(str), str.quoted.length)
//      }
//    }
  }

  "JBoolParser" should "parse true\\false to Boolean correctly" in {
    val boolGen = Gen.oneOf(true, false)
    check {
      forAll(boolGen) {
        bool =>
          parsers.JBoolParser.run(s"$bool") == Right(JBool(bool))
      }
    }
  }


}
