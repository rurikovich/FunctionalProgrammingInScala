package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.RNG
import org.rurik.part2.chapter8.Prop.{FailedCase, SuccessCount, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  type TestCases = Int


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}


case class Prop(run: (TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (n, rng) =>
      val r1 = this.run(n, rng)
      val r2 = p.run(n, rng)
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
    (n, rng) =>
      val r1 = this.run(n, rng)
      val r2 = p.run(n, rng)
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





