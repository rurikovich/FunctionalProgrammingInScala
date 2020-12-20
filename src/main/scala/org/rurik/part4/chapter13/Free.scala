package org.rurik.part4.chapter13

import org.rurik.part2.chapter7.Par.Par
import org.rurik.part3.chapter11.Monad

sealed trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

}


case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]


object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]


  type AToFree[A] = A => Free[Function0, A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {

    def unit[A](a: => A): Free[F, A] = Return(a)

    def flatMap[A, B](ma: F[A])(fn: A => Free[F, B]): Free[F, B] = FlatMap(Suspend(ma), fn)
  }


  // Exercise 2: Implement a specialized `Function0` interpreter.
//  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r: Function0[A]) => r()
    case FlatMap(x, f: AToFree[A]) => x match {
      case Return(a: A) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(a0: Free[Function0, A], g: AToFree[A]) => runTrampoline {
        a0 flatMap {
          aa0: A =>
            val value1: Free[Function0, A] = g(aa0)
            val value: Free[Function0, A] = value1 flatMap f
            value
        }
      }
    }
  }


  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a: A) => F.unit(a)
    case Suspend(r: F[A]) => r
    case FlatMap(Suspend(r: F[A]), f: Function1[A, Free[F, A]]) =>
      F.flatMap(r)((a: A) => run(
        f(a))
      )
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }


}




