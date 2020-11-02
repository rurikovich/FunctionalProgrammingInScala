package org.rurik.part1.part1_5

import org.rurik.part1.part1_5.Stream.{cons, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
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

  def takeUnfold(n: Int): List[A] = unfold((n, this)) {
    case (nn, Cons(h, t)) if (nn > 0) => Some((h(), (nn - 1, t())))
    case _ => None
  }.toList

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

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (p(h())) => Some((h(), t()))
    case _ => None
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

  def map[B](f: A => B): Stream[B] = foldRight(Stream[B]()) {
    (a, b) => cons[B](f(a), b)
  }

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream[A]()) {
    (a, b) => if (f(a)) cons[A](a, b) else b
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream[B]())((a, b) => f(a).append(b))

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some {
      (
        f(h1(), h2()),
        (t1(), t2())
      )
    }
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Cons(h2, t2)) => Some {
      (
        (None, Some(h2())),
        (Empty, t2())
      )
    }

    case (Cons(h1, t1), Empty) => Some {
      (
        (Some(h1()), None),
        (t1(), Empty)
      )
    }

    case (Cons(h1, t1), Cons(h2, t2)) => Some {
      (
        (Some(h1()), Some(h2())),
        (t1(), t2())
      )
    }

    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipWith(s)(_ == _).forAll(identity)

  def tails: Stream[Stream[A]] = {
    val startState: Option[Stream[A]] = Some(this)
    unfold(startState) {
      case Some(Cons(h, t)) => Some((Cons(h, t), Some(t())))
      case Some(Empty) => Some(Empty, None)
      case _ => None
    }
  }

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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantUnfold[A](a: A): Stream[A] = unfold(a) {
    s => Some((s, s))
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = cons(i, go(j, i + j))

    go(0, 1)
  }

  def fibsUnfold(): Stream[Int] = cons(0, unfold((0, 1)) {
    case (f0, f1) => Some(
      (f1, (f1, f0 + f1))
    )
  })

  def fromUnfold(n: Int): Stream[Int] = unfold(n) {
    s => Some((s, s + 1))
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map {
    as => cons(as._1, unfold(as._2)(f))
  }.getOrElse(Stream.empty[A])

  def ones: Stream[Int] = Stream.cons(1, ones)

  def onesUnfold: Stream[Int] = unfold(1)(s => Some(s, s))


}
