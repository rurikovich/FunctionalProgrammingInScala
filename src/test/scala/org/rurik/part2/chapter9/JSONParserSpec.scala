package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.json.{JSON, JsonParser, JsonParsers}
import org.rurik.part2.chapter9.json.JSON.{JBool, JNull, JNumber, JString}
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
    val strGen = Gen.asciiStr

    check {
      forAll(strGen) {
        case (str) =>
          val parser: JsonParser[JSON] = parsers.succeed[JSON](JNull)
          parsers.run(parser)(str) == Right(JNull)
      }
    }
  }

  "JNumberParser" should "parse any number to double correctly" in {
    val numGen = Gen.double

    check {
      forAll(numGen) {
        case (d) =>
          val parser: JsonParser[JSON] = parsers.succeed[JSON](JNull)
          parsers.run(parser)(s"$d") == Right(JNull)
      }
    }
  }

}
