package org.rurik.part2.chapter9.v2

import org.rurik.part2.chapter9.json.JSON
import org.rurik.part2.chapter9.json.JSON.JNull
import org.rurik.part2.chapter9.v2.JsonParsersV2.JsonParser
import org.rurik.part2.chapter9.v2.parsers.{Location, ParseError, ParsersV2}

import scala.util.matching.Regex

class JsonParsersV2 extends ParsersV2[JsonParser] {

  override implicit def string(s: String): JsonParser[String] =
    (loc) => {
      val strToInspect = loc.input.substring(loc.offset)
      if (strToInspect.startsWith(s))
        Success(get = s, charsConsumed = s.length)
      else
        Failure(Location(loc.input, loc.offset).toError("Expected: " + s))
    }

  override implicit def regex(r: Regex): JsonParser[String] = ???

  override def slice[A](p: JsonParser[A]): JsonParser[String] = ???

  def label[A](msg: String)(p: JsonParser[A]): JsonParser[A] = loc => p(loc).mapError(_.label(msg))

  def scope[A](msg: String)(p: JsonParser[A]): JsonParser[A] = loc => p(loc).mapError(_.push(loc, msg))

  def flatMap[A, B](f: JsonParser[A])(g: A => JsonParser[B]): JsonParser[B] =
    loc => f(loc) match {
      case Success(a, n) => g(a)(loc.advanceBy(n)).advanceSuccess(n)
      case e@Failure(_) => e
    }

  def or[A](x: JsonParser[A], y: => JsonParser[A]): JsonParser[A] =
    s => x(s) match {
      case Failure(e) => y(s)
      case r => r
    }

  override def run[A](p: JsonParser[A])(input: String): Either[ParseError, A] = ???

  override def succeed[A](a: A): JsonParser[A] = ???


  def JNullParser: JsonParser[JSON] =
    loc =>
      Success(JNull, 0)

  def JNumberParser: JsonParser[JSON] = ???

  def JStringParser: JsonParser[JSON] = ???

  def JBoolParser: JsonParser[JSON] = ???

  def JArrayParser: JsonParser[JSON] = ???

  def JObjectParser: JsonParser[JSON] = ???


  def AllJsonParser: JsonParser[JSON] = {
    JNullParser or
      JNumberParser or
      JStringParser or
      JBoolParser or
      JArrayParser or
      JObjectParser
  }

}

object JsonParsersV2 {
  type JsonParser[+A] = Location => Result[A]
}




