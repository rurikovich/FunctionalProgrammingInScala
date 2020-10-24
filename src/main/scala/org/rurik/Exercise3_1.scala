package org.rurik

import org.rurik.Exercise3_1.List.{foldRight, sum}

object Exercise3_1 extends App {

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)

  private val value: List[Int] = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  println(value)

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


  }


}

