package org.rurik.part2.chapter9.json

import org.rurik.part2.chapter9.Parsers
import org.rurik.part2.chapter9.json.JSON._
import org.rurik.part2.chapter9.json.JsonParsers.error

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


class JsonParsers extends Parsers[Throwable, JsonParser] {

  import StrHelper._
  import BoolHelper._

  implicit def toJArrayParser(p: JsonParser[List[JSON]]): JsonParser[JSON] = p.map(l => JArray(l.toIndexedSeq))

  override implicit def string(s: String): JsonParser[String] = JsonParser[String](str => Right(s))

  override implicit def regex(r: Regex): JsonParser[String] = ???

  override def run[A](p: JsonParser[A])(input: String): Either[Throwable, A] = p.run(input)

  override def or[A](s1: JsonParser[A], s2: => JsonParser[A]): JsonParser[A] = JsonParser[A] {
    s =>
      s1.run(s).orElse(
        s2.run(s)
      )
  }

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
    s =>
      val tryDouble = Try(s.toDouble).map(JNumber)
      val tryFloat = Try(s.toFloat).map(f => JNumber(f.toDouble))
      val tryInt = Try(s.toInt).map(i => JNumber(i.toDouble))
      tryDouble.orElse(tryFloat).orElse(tryInt).toEither
  }

  val JStringParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      s.withoutFrame(quote) match {
        case Some(str) => Right(JString(str))
        case None => Left(new Exception(error("JString")))
      }
  }

  val JBoolParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      Try(s.toBoolean).toEither.map(JBool)
  }

  val JArrayParser: JsonParser[JSON] = JsonParser[JSON] {
    s => {
      val elements: List[Either[Throwable, JSON]] =
        s.withoutFrameAndDelimeters("[", "]", ",").toList.flatMap {
          _.map(s => AllJsonParser.run(s))
        }

      val startAcc: Either[Throwable, List[JSON]] = Right(List.empty[JSON])
      elements.foldLeft(startAcc) {
        (acc: Either[Throwable, List[JSON]], value: Either[Throwable, JSON]) =>
          for {
            list <- acc
            v <- value
          } yield v :: list

      }.map(l => JArray(l.toIndexedSeq))

    }
  }

  val JObjectParser: JsonParser[JSON] = JsonParser[JSON](
    s => {
      val left = new Exception("JObject parse Error")

      val map: Either[Throwable, Map[String, JSON]] =
        s.withoutFrameAndDelimeters("{", "}", ",").map {
          elements: List[String] =>
            val elems = accumulateTokensInJson(elements)
            elems.flatMap {
              el =>
                val strArr = el.split(":")
                (strArr.length == 2).toOpt().map {
                  _ =>
                    val (name: String, value: String) = (strArr(0).withoutQuotes, strArr(1))
                    (name, AllJsonParser.run(value).getOrElse(JNull))
                }
            }.toMap
        }.toRight(left)

      map.map(JObject)
    }
  )


  val AllJsonParser: JsonParser[JSON] = {
    JNullParser or
      JNumberParser or
      JStringParser or
      JBoolParser or
      JArrayParser or
      JObjectParser
  }


}

object JsonParsers {

  def error(s: String) = s"Failed to parse $s"
}


case class JsonParser[+A](run: String => Either[Throwable, A])