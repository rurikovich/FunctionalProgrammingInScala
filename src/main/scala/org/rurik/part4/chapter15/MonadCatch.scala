package org.rurik.part4.chapter15

import org.rurik.part3.chapter11.Monad

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]

  def fail[A](t: Throwable): F[A]
}
