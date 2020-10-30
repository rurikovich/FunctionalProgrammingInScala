package org.rurik.part1.part1_5

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, tail) => h() :: tail().toList
    case Empty => List.empty[A]
  }

  def take(n:Int):Stream[A]={

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
}
