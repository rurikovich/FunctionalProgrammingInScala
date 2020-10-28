package org.rurik.part_1_4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }


}

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    val m = xs.sum / xs.size

    def f(x: Double): Double = Math.pow(x - m, 2)

    Some(xs.map(f).sum / xs.size)
  }

  def map2[A, B, C](aOpt: Option[A], bOpt: Option[B])(f: (A, B) => C): Option[C] = (aOpt, bOpt) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val emptyList: Option[List[A]] = Some(List.empty[A])
    a.foldLeft(emptyList) {
      (listOpt: Option[List[A]], aOp: Option[A]) =>
        listOpt.flatMap {
          list => aOp.map(a => list ++ List(a))
        }
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val emptyList: Option[List[B]] = Some(List.empty[B])

    a.foldLeft(emptyList) {
      (listOpt: Option[List[B]], aa: A) =>
        for {
          list <- listOpt
          b <- f(aa)
        } yield list ++ List(b)
    }
  }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

