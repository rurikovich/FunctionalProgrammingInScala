package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.JSON.JNull
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class JSONParserSpec extends AnyFlatSpec with Checkers with should.Matchers {

  val parsers: JsonParsers = new JsonParsers()

  "JsonParsers" should "parse str to JSON correctly" in {
    val strGen = Gen.asciiStr

    check {
      forAll(strGen) {
        case (str) =>
          val parser: JsonParser[JSON] = parsers.succeed[JSON](JNull)
          parsers.run(parser)(str) == Right(JNull)
      }
    }

  }

}
