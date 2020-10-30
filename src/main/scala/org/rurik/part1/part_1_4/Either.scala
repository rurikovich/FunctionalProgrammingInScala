package org.rurik.part1.part_1_4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (_, Left(e2)) => Left(e2)
      case (Left(e1), _) => Left(e1)
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
    }

}

object Either {


  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t =>
      traverse(t)(f).map2(f(h))((listB, b) => List(b) ++ listB)
  }


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    val emptyList: Either[E, List[A]] = Right(List.empty[A])
    a.foldLeft(emptyList) {
      (listOpt: Either[E, List[A]], aEither: Either[E, A]) =>
        listOpt.flatMap {
          list => aEither.map(a => list ++ List(a))
        }
    }
  }


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]