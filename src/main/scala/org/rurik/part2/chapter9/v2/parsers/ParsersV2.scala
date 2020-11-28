package org.rurik.part2.chapter9.v2.parsers

import org.rurik.part2.chapter8.Prop.forAll
import org.rurik.part2.chapter8.{Gen, Prop}

import scala.util.matching.Regex


trait ParsersV2[Parser[+_]] {
  self =>

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))


  //--------------------- set of primitives ---------------------------
  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  /*
  осушествляет только подсчет кол-ва симовлов, обработанных парсером, НО не вычисляет результат работы парсера
   */
  def slice[A](p: Parser[A]): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  //--------------------- set of primitives ---------------------------

  def run[A](p: Parser[A])(input: String): Result[A]

  def succeed[A](a: A): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    map2(p, listOfN(n - 1, p))(_ :: _) or succeed(List())
  }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap {
    a => succeed(f(a))
  }

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)


  /** Sequences two parsers, ignoring the result of the first.
   * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  /** Sequences two parsers, ignoring the result of the second.
   * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def slice(): Parser[String] = self.slice(p)

    def many(): Parser[List[A]] = self.many(p)

    def many1(): Parser[List[A]] = self.many1(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def run(input: String): Result[A] = self.run(p)(input)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*[B](p2: => Parser[B]): Parser[A] = self.skipR(p, p2)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)


    def productLaw[A, B](p: Parser[A])(in: Gen[(B, String)]): Prop = {
      implicit def toEither[C](r: Result[C]): Either[ParseError, C] = r match {
        case Success(a: C, _) => Right(a)
        case Failure(e) => Left(e)
      }

      forAll(in) {
        case (b, s) =>
          val parserBA = succeed(b) ** p
          toEither(run(parserBA)(s)) == run(p)(s).map(a => (b, a))
      }
    }

  }

}





