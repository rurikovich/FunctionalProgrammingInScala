package org.rurik.part1.part1_3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] =
    list match {
      case Cons(_, tail) => tail
      case _ => Nil
    }


  def setHead[A](x: A, list: List[A]): List[A] = Cons(x, tail(list))

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(n: Int, l: List[A]): List[A] = n match {
      case 1 => tail(l)
      case _ => go(n - 1, tail(l))
    }

    go(n, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, tail) if f(x) => dropWhile(tail, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
      case Cons(x, tail) => Cons(x, init(tail))
    }


  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  def length[A](as: List[A]): Int =
    foldRight[A, Int](as, 0)((_, b) => 1 + b)


  def sumFL(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productFL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)


  def lengthFL[A](as: List[A]): Int =
    foldLeft[A, Int](as, 0)((b, _) => 1 + b)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((list: List[A], a: A) => Cons(a, list))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, list) => Cons(a, list))

  def concat[A](ll: List[List[A]]): List[A] = foldLeft(ll, List[A]())(
    (accList, list) => append(accList, list)
  )

  def add1(l: List[Int]): List[Int] =
    foldLeft(reverse(l), Nil: List[Int]) {
      (list, i) => Cons(i + 1, list)
    }

  def doubleToString(l: List[Double]): List[String] = {
    foldLeft(reverse(l), Nil: List[String]) {
      (list, d) => Cons(d.toString, list)
    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldLeft(reverse(l), Nil: List[B]) {
      (list, a) => Cons(f(a), list)
    }
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldLeft(reverse(l), List[A]()) {
    (l, a) => {
      if (f(a)) Cons(a, l) else l
    }
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldLeft(reverse(l), Nil: List[B]) {
      (list, a) => append(f(a), list)
    }
  }

  def filterFM[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) {
    a => if (f(a)) List(a) else List()
  }

  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumLists(xs, ys))
  }


  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def hasSubsequence[A](l: List[A], originSub: List[A]): Boolean = {
    @annotation.tailrec
    def go(l: List[A], curSub: List[A]): Boolean =
      (l, curSub) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x == y) go(xs, ys) else go(xs, originSub)
      }

    go(l, originSub)
  }


}