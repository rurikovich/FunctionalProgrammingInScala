package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.{RNG, SimpleRNG}
import org.rurik.part2.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  type TestCases = Int

  type MaxSize = Int


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i).unsized)(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }


  def run(p: Prop,
          maxSize: Int = 10,
          testCases: Int = 10,
          rng: RNG = SimpleRNG(System.currentTimeMillis)
         ): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
    case Passed => println(s"+ OK, passed $testCases tests.")
  }

}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      val r1 = this.run(max, n, rng)
      val r2 = p.run(max, n, rng)
      (r1, r2) match {
        case (Passed, Passed) => Passed
        case (Passed, Falsified(failure, successes)) => Falsified(failure, successes)
        case (Falsified(failure, successes), Passed) => Falsified(failure, successes)
        case (Falsified(failure1, successes1), Falsified(failure2, successes2)) =>
          Falsified(
            failure = s"prop1:${failure1} prop2:${failure2}",
            successes = successes1 + successes2
          )
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      val r1 = this.run(max, n, rng)
      val r2 = p.run(max, n, rng)
      (r1, r2) match {
        case (Passed, Passed) => Passed
        case (Passed, _) => Passed
        case (_, Passed) => Passed
        case (Falsified(failure, successes), _) => Falsified(failure, successes)
      }
  }

}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}





