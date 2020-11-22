package org.rurik.part2.chapter9

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


class JsonParsers extends Parsers[Exception, JsonParser] {

  override implicit def string(s: String): JsonParser[String] = JsonParser[String](str => Right(s))

  override implicit def regex(r: Regex): JsonParser[String] = ???

  override def run[A](p: JsonParser[A])(input: String): Either[Exception, A] = p.run(input)

  override def or[A](s1: JsonParser[A], s2: => JsonParser[A]): JsonParser[A] = ???

  override def slice[A](p: JsonParser[A]): JsonParser[String] = ???

  override def flatMap[A, B](p: JsonParser[A])(f: A => JsonParser[B]): JsonParser[B] =
    JsonParser[B](
      (s: String) => p.run(s).map(f).flatMap(_.run(s))
    )


}


case class JsonParser[+A](run: String => Either[Exception, A])