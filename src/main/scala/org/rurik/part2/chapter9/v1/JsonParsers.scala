package org.rurik.part2.chapter9.v1

import org.rurik.part2.chapter9.json.JSON._
import org.rurik.part2.chapter9.json.JSON
import org.rurik.part2.chapter9.v1.JsonParsers.error

import scala.util.Try
import scala.util.matching.Regex

class JsonParsers extends Parsers[Throwable, JsonParser] {

  import org.rurik.part2.chapter9.helpers.StrHelper._

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

  def JNullParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      if (s.isEmpty || s == "null") Right(JNull) else Left(new Exception(error("JNull")))
  }

  def JNumberParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      val tryDouble = Try(s.toDouble).map(JNumber)
      val tryFloat = Try(s.toFloat).map(f => JNumber(f.toDouble))
      val tryInt = Try(s.toInt).map(i => JNumber(i.toDouble))
      tryDouble.orElse(tryFloat).orElse(tryInt).toEither
  }

  def JStringParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      s.withoutFrame(quote) match {
        case Some(str) => Right(JString(str))
        case None => Left(new Exception(error("JString")))
      }
  }

  def JBoolParser: JsonParser[JSON] = JsonParser[JSON] {
    s =>
      Try(s.toBoolean).toEither.map(JBool)
  }

  def JArrayParser: JsonParser[JSON] = JsonParser[JSON] {
    val left: Either[Throwable, JSON] = Left(new Exception("JArray parse Error"))

    s => {
      val elementsOpt: Option[List[Either[Throwable, JSON]]] =
        s.withoutFrameAndDelimeters("[", "]", ",").map {
          elements: List[String] =>
            elements.map(s => AllJsonParser.run(s))
        }

      val option: Option[Either[Throwable, List[JSON]]] = elementsOpt.map {
        elements =>
          val startAcc: Either[Throwable, List[JSON]] = Right(List.empty[JSON])
          elements.foldLeft(startAcc) {
            (acc: Either[Throwable, List[JSON]], value: Either[Throwable, JSON]) =>
              for {
                list <- acc
                v <- value
              } yield list ++ List(v)

          }
      }

      option match {
        case Some(either) => either.map(l => JArray(l.toIndexedSeq))
        case None => left
      }

    }
  }

  def JObjectParser: JsonParser[JSON] = JsonParser[JSON](
    s => {
      val left = new Exception("JObject parse Error")

      val map: Either[Throwable, Map[String, JSON]] =
        s.withoutFrameAndDelimeters("{", "}", ",").map {
          elements: List[String] =>
            val elems = accumulateTokensInJson(elements)
            elems.flatMap {
              el =>
                val strArr = el.split(":").toList
                strArr.headOption.map {
                  head =>
                    val (name: String, value: String) = (head.withoutQuotes(), strArr.tail.mkString(":"))
                    val innerResult: Either[Throwable, JSON] = AllJsonParser.run(value)
                    (name, innerResult.getOrElse(JNull))
                }

            }.toMap
        }.toRight(left)

      map.map(JObject)
    }
  )


  def AllJsonParser: JsonParser[JSON] = {
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