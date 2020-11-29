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

  "JSONParserV2: succeed" should "parse any str to constant value correctly" in {
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

  "JSONParserV2: JNullParser" should "parse \"null\" to JNull correctly" in {
    val nullStr = "null"
    JNullParser.run(nullStr) shouldEqual Success(JNull, nullStr.length)
  }

  "JSONParserV2: JNumberParser" should "parse any number to double correctly" in {

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

  "JSONParserV2: JStringParser" should "parse any quoted string to string correctly" in {

    val str1 = ""
    val quotedStr1 = str1.quoted
    JStringParser.run(quotedStr1) shouldEqual Success(JString(str1), quotedStr1.length)

    val strGen = Gen.stringOfN(10, Gen.alphaLowerChar)
    check {
      forAll(strGen) {
        str =>
          JStringParser.run(str.quoted) == Success(JString(str), str.quoted.length)
      }
    }
  }

  "JSONParserV2: JBoolParser" should "parse true\\false to Boolean correctly" in {
    val boolGen = Gen.oneOf(true, false)
    check {
      forAll(boolGen) {
        bool =>
          JBoolParser.run(s"$bool") == Success(JBool(bool), bool.toString.length)
      }
    }
  }

  "JSONParserV2: JArrayParser" should "parse [...] to array correctly" in {

    val list = List(1)
    val arrStr = list.mkString("[", ",", "]")
    val jArrayToCheck: JArray = JArray(list.map(i => JNumber(i)).toIndexedSeq)
    JArrayParser.run(arrStr) shouldEqual Success(jArrayToCheck, arrStr.length)

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
    val arrGen: Gen[List[A]] = Gen.nonEmptyListOf[A](Gen.const(constValueToGen))
    check {
      forAll(arrGen) {
        list =>
          val jArrayToCheck: JArray = JArray(IndexedSeq.fill(list.size)(constValueToCheck).map(fn))
          val arrStr = list.mkString("[", ",", "]")
          val value1 = JArrayParser.run(arrStr)
          value1 == Success(jArrayToCheck, arrStr.length)
      }
    }
  }

  "JSONParserV2: JObjectParser" should "parse simple json correctly" in {
    import parsers._

    val json =
      """
        |{
        |"name1":1
        |}
        |""".stripMargin.replace("\n", "")

    val value1 = JObjectParser.run(json)
    value1 shouldEqual Success(JObject(Map("name1" -> JNumber(1))), json.length)
  }

  "JSONParserV2: JObjectParser" should "parse json with array correctly" in {
    import parsers._
    val json =
      """
        |{
        |"name1":1,
        |"name2":[true,false,true],
        |"name3":"aaa"
        |}
        |""".stripMargin.replace("\n", "")

    JObjectParser.run(json) shouldEqual Success(
      get = JObject(Map(
        "name1" -> JNumber(1),
        "name2" -> JArray(IndexedSeq(JBool(true), JBool(false), JBool(true))),
        "name3" -> JString("aaa")
      )),
      charsConsumed = json.length
    )

  }

  "JSONParserV2: JObjectParser" should "parse json with inner jObject correctly" in {
    import parsers._
    val json =
      """
        |{
        |"name1":1,
        |"name2":{
        |"name21":1,
        |"name22":true
        |},
        |"name3":"aaa"
        |}
        |""".stripMargin.replace("\n", "")

    val parseResult = JObjectParser.run(json)

    parseResult shouldEqual Success(
      get = JObject(Map(
        "name1" -> JNumber(1),
        "name2" -> JObject(Map(
          "name21" -> JNumber(1),
          "name22" -> JBool(true)
        )),
        "name3" -> JString("aaa")
      )),
      charsConsumed = json.length
    )

  }

  "JSONParserV2: JObjectParser" should "parse json with inner inner jObject  and array correctly" in {
    import parsers._
    val json =
      """
        |{
        |"name1":1,
        |"name2":{
        |"name21":{
        |"name211":111
        |},
        |"name22":[true,false,true]
        |},
        |"name3":"aaa"
        |}
        |""".stripMargin.replace("\n", "")

    val parseResult = JObjectParser.run(json)
    parseResult shouldEqual Success(
      get = JObject(Map(
        "name1" -> JNumber(1),
        "name2" -> JObject(Map(
          "name21" -> JObject(Map("name211" -> JNumber(111))),
          "name22" -> JArray(IndexedSeq(JBool(true), JBool(false), JBool(true)))
        )),
        "name3" -> JString("aaa")
      )),
      charsConsumed = json.length
    )

  }

  "JSONParserV2: JArrayParser" should "parse array of jObjects correctly" in {
    import parsers._
    val json =
      """
        |[
        |{
        |"name1":1
        |},
        |{
        |"name2":2
        |},
        |{
        |"name3":3
        |}
        |]
        |""".stripMargin.replace("\n", "")

    val parseResult = JArrayParser.run(json)
    parseResult shouldEqual Success(
      get = JArray(IndexedSeq(
        JObject(Map("name1" -> JNumber(1))),
        JObject(Map("name2" -> JNumber(2))),
        JObject(Map("name3" -> JNumber(3)))
      )),
      charsConsumed = json.length
    )

  }

  "JSONParserV2: JObjectParser" should "parse empty json  correctly" in {
    import parsers._
    val json =
      """
        |{
        |}
        |""".stripMargin.replace("\n", "")

    val parseResult = JObjectParser.run(json)
    parseResult shouldEqual Success(JObject(Map.empty), json.length)
  }

  "JSONParserV2: JArrayParser" should "parse empty array  correctly" in {
    import parsers._
    val json = "[]"
    JArrayParser.run(json) shouldEqual Success(JArray(IndexedSeq.empty), json.length)
  }

}
