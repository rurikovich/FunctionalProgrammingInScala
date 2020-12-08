package org.rurik.part3.chapter12

import org.rurik.part3.chapter10.{Foldable, Monoid}
import org.rurik.part3.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

}

object Traverse {

  def listTraverse: Traverse[List] = new Traverse[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }


  def optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  def treeTraverse: Traverse[Tree] = new Traverse[Tree] {

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      (as.head, as.tail) match {
        case (h, Nil) => f(h, z)
        case (h: A, t: List[Tree[A]]) =>
          t.foldRight(f(h, z)) {
            (tree: Tree[A], acc: B) =>
              foldRight(tree)(acc)(f)
          }
      }


    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = (as.head, as.tail) match {
      case (h, Nil) => f(z, h)
      case (h: A, t: List[Tree[A]]) =>
        t.foldLeft(f(z, h)) {
          (acc: B, tree: Tree[A]) =>
            foldLeft(tree)(acc)(f)
        }
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa.copy(f(fa.head), fa.tail.map(t => map(t)(f)))
  }


}
