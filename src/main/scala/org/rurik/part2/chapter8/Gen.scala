package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.{RNG, State}

case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](State(RNG.nonNegativeBetween(start, stopExclusive)(_)))
  }

}
