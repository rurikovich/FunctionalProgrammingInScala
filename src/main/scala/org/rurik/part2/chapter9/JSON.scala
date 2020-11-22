package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.JSON.{JArray, JBool, JNull, JNumber, JString}

import scala.util.Try
import scala.util.matching.Regex

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}


class JsonParsers extends Parsers[Throwable, JsonParser] with StrHelper {

  implicit def toJArrayParser(p: JsonParser[List[JSON]]): JsonParser[JSON] = p.map(l => JArray(l.toIndexedSeq))

  override implicit def string(s: String): JsonParser[String] = JsonParser[String](str => Right(s))

  override implicit def regex(r: Regex): JsonParser[String] = ???

  override def run[A](p: JsonParser[A])(input: String): Either[Throwable, A] = p.run(input)

  override def or[A](s1: JsonParser[A], s2: => JsonParser[A]): JsonParser[A] = ???

  override def slice[A](p: JsonParser[A]): JsonParser[String] = ???

  override def flatMap[A, B](p: JsonParser[A])(f: A => JsonParser[B]): JsonParser[B] =
    JsonParser[B](
      (s: String) => p.run(s).map(f).flatMap(_.run(s))
    )

  override def succeed[A](a: A): JsonParser[A] = JsonParser[A](s => Right(a))

  val JNullParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      if (s == "null") Right(JNull) else Left(new Exception(error("JNull")))
  }

  val JNumberParser: JsonParser[JSON] = JsonParser[JSON] {
    s => Try(s.toDouble).toEither.map(JNumber)
  }

  val JStringParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      if (s.startsWith("\"") && s.endsWith("\""))
        Right(JString(strWithoutQuotes(s)))
      else
        Left(new Exception(error("JString")))
  }

  val JBoolParser: JsonParser[JSON] = JsonParser[JSON] {
    s => Try(s.toBoolean).toEither.map(JBool)
  }

  val JArrayParser: JsonParser[JSON] = JNumberParser.many or JStringParser.many or JObjectParser.many

  val JObjectParser: JsonParser[JSON] = ???


  val AllJsonParser: JsonParser[JSON] = {
    JNullParser or JNumberParser or
      JStringParser or JBoolParser or
      JArrayParser or JObjectParser
  }

  def error(s: String) = s"Failed to parse $s"

}


case class JsonParser[+A](run: String => Either[Throwable, A])