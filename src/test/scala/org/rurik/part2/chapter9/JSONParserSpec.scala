package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.json.{JSON, JsonParser, JsonParsers}
import org.rurik.part2.chapter9.json.JSON.{JArray, JBool, JNull, JNumber, JObject, JString}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class JSONParserSpec extends AnyFlatSpec with Checkers with should.Matchers {

  import org.rurik.part2.chapter9.json.StrHelper._

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

  "JNullParser" should "parse \"null\" to JNull correctly" in {
    parsers.JNullParser.run("null") shouldEqual Right(JNull)
  }

  "JNumberParser" should "parse any number to double correctly" in {
    val numGen = Gen.double
    check {
      forAll(numGen) {
        d => parsers.JNumberParser.run(s"$d") == Right(JNumber(d))
      }
    }
  }

  "JStringParser" should "parse any quoted string to string correctly" in {
    val strGen = Gen.stringOfN(10, Gen.asciiPrintableChar)
    check {
      forAll(strGen) {
        str =>
          parsers.JStringParser.run(str.quoted) == Right(JString(str))
      }
    }
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


  "AllJsonParser" should "parse any type  correctly" in {
    parsers.AllJsonParser.run("null") shouldEqual Right(JNull)
    parsers.AllJsonParser.run("1") shouldEqual Right(JNumber(1))
    parsers.AllJsonParser.run("aaa".quoted) shouldEqual Right(JString("aaa"))
    parsers.AllJsonParser.run("true") shouldEqual Right(JBool(true))
  }

  "JArrayParser" should "parse [...] to array correctly" in {
    checkJArray[Int](
      constValueToGen = 1,
      constValueToCheck = 1,
      fn = (i: Int) => JNumber(i)
    )

    checkJArray[String](
      constValueToGen = "a".quoted,
      constValueToCheck = "a",
      fn = (s: String) => JString(s)
    )

    checkJArray[Boolean](
      constValueToGen = false,
      constValueToCheck = false,
      fn = (b: Boolean) => JBool(b)
    )

  }


  private def checkJArray[A](constValueToGen: A, constValueToCheck: A, fn: A => JSON): Any = {
    import parsers._
    val arrGen: Gen[List[A]] = Gen.listOf[A](Gen.const(constValueToGen))
    check {
      forAll(arrGen) {
        list =>
          val jArrayToCheck = JArray(IndexedSeq.fill(list.size)(constValueToCheck).map(fn))
          val arrStr = list.mkString("[", ",", "]")
          JArrayParser.run(arrStr) == Right(jArrayToCheck)
      }
    }
  }

  "JObjectParser" should "parse simple json correctly" in {
    import parsers._
    val json =
      """
        |{
        |"name1":1
        |}
        |""".stripMargin
    JObjectParser.run(json) shouldEqual Right(JObject(Map("name1" -> JNumber(1))))
  }

  "JObjectParser" should "parse json with array correctly" in {
    import parsers._
    val json =
      """
        |{
        |"name1":1,
        |"name2":[true,false,true],
        |"name3":"aaa"
        |}
        |""".stripMargin

    JObjectParser.run(json) shouldEqual Right(JObject(Map(
      "name1" -> JNumber(1),
      "name2" -> JArray(IndexedSeq(JBool(true), JBool(false), JBool(true))),
      "name3" -> JString("aaa"),
    )))

  }


}
