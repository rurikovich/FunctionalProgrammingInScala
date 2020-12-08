package org.rurik.part3.chapter12

import org.rurik.part3.chapter10.{Foldable, Monoid}
import org.rurik.part3.chapter11.{Functor, Monad}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

}

object Traverse {

  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      oa match {
        case Some(a) => M.map(f(a))(Some(_))
        case None => M.unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }


}
