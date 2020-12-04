package org.rurik.part3.chapter11

import org.rurik.part1.part1_4.{Option, Some}
import org.rurik.part1.part1_5
import org.rurik.part2.chapter7.Par
import org.rurik.part2.chapter7.Par.Par
import org.rurik.part2.chapter8.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldLeft(unit(List.empty[A])) {
      (mList: F[List[A]], m: F[A]) =>
        map2(mList, m)(_ ++ List(_))
    }
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    val acc: F[List[B]] = unit(List.empty[B])
    la.foldLeft(acc) {
      (mLIst: F[List[B]], a: A) =>
        map2(mLIst, f(a))(_ ++ List(_))
    }
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    traverse((0 to n).toList)(_ => ma)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))


  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    (a: A) => flatMap(f(a))(g)


  def flatMapC[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    compose[Unit, A, B](_ => ma, f)(())
  }


  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val value: F[List[A]] = unit(List[A]())
    ms.foldRight(value) {
      (a: A, mList: F[List[A]]) => {
        val fBool: F[Boolean] = f(a)
        map2(fBool, mList) {
          (bool: Boolean, list: List[A]) =>
            if (bool) a :: list else list
        }
      }
    }


  }


  trait Laws {
    type A

    def f: A => F[A]

    def x: F[A]


    compose(f, unit[A]) == f

    (a: A) => {
      val xx: F[A] = f(a)
      flatMap(xx)(unit[A]) == f
    }

    flatMap(x)(unit[A]) == x


  }

}

object Monad {


  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
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
    override def unit[A](a: => A): List[A] = List.empty

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }


  val idLaw = optionMonad.flatMap(Some(1))(optionMonad.unit) == Some(1)


}
