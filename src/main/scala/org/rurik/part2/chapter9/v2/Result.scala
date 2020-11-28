package org.rurik.part2.chapter9.v2

import org.rurik.part2.chapter9.v2.parsers.ParseError

sealed trait Result[+A] {

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e) => Failure(f(e))
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }

}


case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

case class Failure(get: ParseError) extends Result[Nothing]

