package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.RNG.Rand
import org.rurik.part1.part1_6.{RNG, State}

case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](State(RNG.nonNegativeBetween(start, stopExclusive)(_)))
  }

  def unit[A](a: => A): Gen[A] = Gen[A](State(RNG.unit(a)(_)))

  def boolean: Gen[Boolean] = Gen[Boolean](State(RNG.map(RNG.int)(_ > 0)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](
    State(
      (rnd: RNG) => {
        val randList: List[Rand[A]] = (0 until n).map {
          _ => (r: RNG) => g.sample.run(r)
        }.toList

        RNG.sequence(randList)(rnd)
      }
    )
  )
}
