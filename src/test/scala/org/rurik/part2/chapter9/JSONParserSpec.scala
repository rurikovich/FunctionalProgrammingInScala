package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.json.{JSON, JsonParser, JsonParsers}
import org.rurik.part2.chapter9.json.JSON.{JBool, JNull, JNumber, JString}
import org.rurik.part2.chapter9.json.JsonParsers.error
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class JSONParserSpec extends AnyFlatSpec with Checkers with should.Matchers {

  val parsers: JsonParsers = new JsonParsers()

  "succeed" should "parse any str to constant value correctly" in {
    val strGen = Gen.asciiStr
    val jsonGen = Gen.oneOf(JNull, JNumber(1), JString(""), JBool(true))

    check {
      forAll(strGen, jsonGen) {
        case (str, json) =>
          val parser: JsonParser[JSON] = parsers.succeed[JSON](json)
          parsers.run(parser)(str) == Right(json)
      }
    }
  }

  "JNullParser" should "parse any str to JNull correctly" in {
    parsers.JNullParser.run("null") shouldEqual Right(JNull)
  }

  "JNumberParser" should "parse any number to double correctly" in {
    val numGen = Gen.double
    check {
      forAll(numGen) {
        case (d) =>
          parsers.JNumberParser.run(s"$d") == Right(JNumber(d))
      }
    }
  }

}
