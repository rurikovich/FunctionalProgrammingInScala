package org.rurik.part2.chapter8

object PropertyTesting {

  def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  def max(l: List[Int]): Int = l.foldLeft(Int.MinValue)(Math.max)

}
