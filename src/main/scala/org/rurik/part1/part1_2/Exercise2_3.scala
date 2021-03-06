package org.rurik.part1.part1_2

object Exercise2_3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a) => {
      (b) => f(a, b)
    }

  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }

}
