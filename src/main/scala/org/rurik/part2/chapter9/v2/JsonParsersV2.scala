package org.rurik.part2.chapter9.v2

import org.rurik.part2.chapter9.json.JSON
import org.rurik.part2.chapter9.json.JSON.{JArray, JBool, JNull, JNumber, JObject, JString}
import org.rurik.part2.chapter9.v2.JsonParsersV2.JsonParser
import org.rurik.part2.chapter9.v2.RegexPatterns._
import org.rurik.part2.chapter9.v2.parsers._

import scala.util.matching.Regex

class JsonParsersV2 extends ParsersV2[JsonParser] {

  import org.rurik.part2.chapter9.helpers.StrHelper._

  implicit def string(s: String): JsonParser[String] = scope(s"string($s)") {
    (loc) => {
      val strToInspect = loc.input.substring(loc.offset)
      if (strToInspect.startsWith(s))
        Success(get = s, charsConsumed = s.length)
      else
        Failure(Location(loc.input, loc.offset).toError("Expected: " + s))
    }
  }

  implicit def regex(r: Regex): JsonParser[String] = scope("regex") {
    loc => {
      val msg = "regex " + r
      val strToInspect = loc.input.substring(loc.offset)
      r.findPrefixMatchOf(strToInspect) match {
        case None =>
          Failure(loc.toError(msg))
        case Some(m: Regex.Match) =>
          val matchedStr = m.toString
          Success(matchedStr, matchedStr.length)
      }
    }
  }

  def slice[A](p: JsonParser[A]): JsonParser[String] = scope("slice") {
    loc =>
      p(loc) match {
        case Success(_, n) =>
          val newStr = loc.input.substring(loc.offset, loc.offset + n)
          Success(newStr, n)
        case f@Failure(_) => f
      }
  }

  def label[A](msg: String)(p: JsonParser[A]): JsonParser[A] = loc => p(loc).mapError(_.label(msg))

  def scope[A](msg: String)(p: JsonParser[A]): JsonParser[A] = loc => p(loc).mapError(_.push(loc, msg))

  def flatMap[A, B](f: JsonParser[A])(g: A => JsonParser[B]): JsonParser[B] = scope(s"flatMap f=${f.toString()}") {
    loc =>
      f(loc) match {
        case Success(a, n) => g(a)(loc.advanceBy(n)).advanceSuccess(n)
        case e@Failure(_) => e
      }
  }

  def or[A](x: JsonParser[A], y: => JsonParser[A]): JsonParser[A] = scope("or") {
    s =>
      x(s) match {
        case Failure(e) => y(s)
        case r => r
      }
  }

  def run[A](p: JsonParser[A])(input: String): Result[A] = p(Location(input))


  def succeed[A](a: A): JsonParser[A] = scope("succeed") {
    _ => Success(a, 0)
  }

  def JNullParser: JsonParser[JSON] = scope("JNullParser")(
    string("null").map(_ => JNull)
  )

  def JNumberParser: JsonParser[JSON] = scope("JNumberParser")(
    regex(numberPattern).map(n => JNumber(n.toDouble))
  )

  def JStringParser: JsonParser[JSON] = scope("JStringParser")(
    AnyString.map(JString)
  )

  def AnyString: JsonParser[String] = scope("AnyString")(
    regex(stringPattern).map(_.withoutQuotes())
  )

  def JBoolParser: JsonParser[JSON] = scope("JBoolParser")(
    ("true" or "false").map(_.toBoolean).map(JBool)
  )

  def JArrayParser: JsonParser[JSON] = scope("JArrayParser")(
    ("[" *> (literal sep ",") <* "]").map(list => JArray(list.toIndexedSeq))
  )

  def JObjectProp: JsonParser[(String, JSON)] = scope("JObjectProp")(
    (AnyString <* ":") ** literal
  )

  def JObjectParser: JsonParser[JObject] = scope("JObject")(
    ("{" *> (JObjectProp sep ",") <* "}").map(_.toMap).map(JObject)
  )


  def allParsers: JsonParser[JSON] = scope("allParsers") {
    JNullParser or
      JNumberParser or
      JStringParser or
      JBoolParser or
      JArrayParser or
      JObjectParser
  }

  def literal: JsonParser[JSON] = scope("literal") {
    JNullParser or
      JNumberParser or
      JStringParser or
      JBoolParser or
      JObjectParser
  }


}

object JsonParsersV2 {
  type JsonParser[+A] = Location => Result[A]
}




