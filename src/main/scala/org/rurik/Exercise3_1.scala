package org.rurik

import org.rurik.List._

object Exercise3_1 extends App {

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  println(x)

  private val value: List[Int] = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  println(value)




}

