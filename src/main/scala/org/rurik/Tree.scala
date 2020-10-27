package org.rurik

import java.lang.Math._

import org.rurik.Tree.fold

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case _: Leaf[A] => 1
    case b: Branch[A] => size(b.left) + size(b.right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case l: Leaf[Int] => l.value
    case b: Branch[Int] => max(maximum(b.left), maximum(b.right))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case _: Leaf[A] => 1
    case b: Branch[A] => max(depth(b.left), depth(b.right)) + 1
  }

  def fold[A, B](t: Tree[A])(leafFn: Leaf[A] => B)(branchFn: (B, B) => B): B = {
    t match {
      case l: Leaf[A] => leafFn(l)
      case b: Branch[A] => branchFn(
        fold(b.left)(leafFn)(branchFn), fold(b.right)(leafFn)(branchFn)
      )
    }
  }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def maximumF(t: Tree[Int]): Int = fold(t)(_.value)(max)

  def depthF[A](t: Tree[A]): Int = fold(t)(_ => 1)(max(_, _) + 1)
}
