package org.rurik.part1.part1_5

import org.rurik.part1.part1_5.Stream.cons

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def toList: List[A] = {

    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] =
      s match {
        case Cons(h, tail) => go(tail(), h() :: acc)
        case _ => acc
      }

    go(this, List()).reverse
  }

  def take(n: Int): List[A] = (n, this) match {
    case (0, _) => List.empty[A]
    case (_, Empty) => List.empty[A]
    case (_, Cons(h, tail)) => h() :: tail().take(n - 1)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = (n, this) match {
    case (0, t) => t
    case (_, Empty) => Empty
    case (_, Cons(_, t)) => t().drop(n - 1)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      if (p(h())) cons(h(), t().takeWhile(p)) else Empty
    case Empty => Empty
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(Stream[A]()) {
    (a, b) => if (p(a)) cons(a, b) else b
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case Empty => true
    }
  }

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

//  def map[B](l: Stream[A])(f: A => B): Stream[B] = ???
//
//  def append(l: Stream[A], r: Stream[A]): Stream[A] = ???


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
