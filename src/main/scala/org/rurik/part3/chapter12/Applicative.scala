package org.rurik.part3.chapter12

import org.rurik.part3.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))


  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldLeft(unit(List.empty[A])) {
      (mList: F[List[A]], m: F[A]) =>
        map2(mList, m)(_ ++ List(_))
    }
  }


  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = traverse((0 to n).toList)(_ => ma)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

}
