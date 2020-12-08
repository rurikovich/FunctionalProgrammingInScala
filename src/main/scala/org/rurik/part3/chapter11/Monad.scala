package org.rurik.part3.chapter11

import org.rurik.part1.part1_4.{Option, Some}
import org.rurik.part1.part1_5
import org.rurik.part1.part1_6.State
import org.rurik.part2.chapter7.Par
import org.rurik.part2.chapter7.Par.Par
import org.rurik.part2.chapter8.Gen
import org.rurik.part3.chapter12.Applicative


trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A,B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}


object Monad {


  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }


  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }


  val streamMonad = new Monad[part1_5.Stream] {
    override def unit[A](a: => A): part1_5.Stream[A] = part1_5.Stream.apply(a)

    override def flatMap[A, B](ma: part1_5.Stream[A])(f: A => part1_5.Stream[B]): part1_5.Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }


  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }


  def unitOptFn[A]: A => Option[A] = (a: A) => optionMonad.unit(a)

  val idLaw = optionMonad.flatMap(Some(1))(unitOptFn[Int]) == Some(1)


  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] = {
    val startAcc: State[Int, List[(Int, A)]] = F.unit(List[(Int, A)]())

    val state: State[Int, List[(Int, A)]] =
      as.foldLeft(startAcc) {
        (acc, a) =>
          for {
            xs <- acc
            n <- getState
            _ <- setState(n + 1)
          } yield (n, a) :: xs
      }
    state.run(0)._1.reverse

  }


  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
  }

}
