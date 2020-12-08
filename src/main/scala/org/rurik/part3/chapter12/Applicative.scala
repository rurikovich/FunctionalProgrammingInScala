package org.rurik.part3.chapter12

import org.rurik.part3.chapter10.Monoid
import org.rurik.part3.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  self =>

  // primitive combinators
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply[A, B](unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fABC: F[A => B => C] = unit(f.curried)
    val fBC: F[B => C] = apply(fABC)(fa)
    apply(fBC)(fb)
  }

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fnABCD: A => B => C => D = f.curried
    val fBCD: F[B => C => D] = map(fa)(fnABCD)
    val fCD: F[C => D] = apply(fBCD)(fb)
    apply(fCD)(fc)
  }


  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fnABCDE: A => B => C => D => E = f.curried
    val fBCDE: F[B => C => D => E] = map(fa)(fnABCDE)
    val fCDE: F[C => D => E] = apply(fBCDE)(fb)
    val fDE: F[D => E] = apply(fCDE)(fc)
    apply(fDE)(fd)
  }


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

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    }


  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {

    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] = self.map2(fab, fa) {
      (gAB: G[A => B], gA: G[A]) =>
        G.map2(gAB, gA) { (ab: A => B, a: A) => ab(a) }
    }

  }


  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldLeft(unit(Map.empty[K, V])) {
      (accMap: F[Map[K, V]], kv: (K, F[V])) =>
        map2(accMap, kv._2) {
          (m: Map[K, V], v: V) =>
            m + (kv._1 -> v)
        }
    }
  }

}

object Applicative {


  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {

    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (faS: Success[A], fbS: Success[B]) => Success(f(faS.a, fbS.a))

        case (faF: Failure[E], _: Success[B]) => faF

        case (_: Success[A], fbF: Failure[E]) => fbF

        case (faF: Failure[E], fbF: Failure[E]) =>
          Failure[E](
            head = faF.head,
            tail = faF.tail ++ Vector(fbF.head) ++ fbF.tail
          )
      }
    }

    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
      (fab, fa) match {
        case (fabS: Success[A => B], faS: Success[A]) => Success(fabS.a(faS.a))

        case (fabF: Failure[E], _: Success[A]) => fabF

        case (_: Success[A => B], faF: Failure[E]) => faF

        case (fabF: Failure[E], faF: Failure[E]) =>
          Failure[E](
            head = fabF.head,
            tail = fabF.tail ++ Vector(faF.head) ++ faF.tail
          )
      }


  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

}
